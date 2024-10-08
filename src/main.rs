use std::{
    fs::{remove_file, File},
    io::Write,
    net,
    path::Path,
};

use elm_rs::{Elm, ElmDecode};
use mpd::{error, song::QueuePlace, Client, Id, State, Status};
use poem::{
    get, handler, http::Method, listener::TcpListener, middleware::Cors, post, web::Json,
    EndpointExt, Route, Server,
};
use serde::Serialize;

const ELM_CONNECTION_RESULT: &str = "
type ConnectionResultWrapper a
    = MpdError MpdError
    | MpdOk a


connectionResultWrapperDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder (ConnectionResultWrapper a)
connectionResultWrapperDecoder decoder = 
    Json.Decode.oneOf
        [ Json.Decode.map MpdError (Json.Decode.field \"MpdError\" (mpdErrorDecoder))
        , Json.Decode.map MpdOk (Json.Decode.field \"MpdOk\" (decoder))
        ]
\n\n";

type Mpd = Client<net::TcpStream>;
pub fn mpd() -> error::Result<Mpd> {
    Ok(Client::connect("127.0.0.1:6600")?)
}

#[derive(Serialize, Elm, ElmDecode)]
enum MpdError {
    /// Used on the client to represent the API not responding correctly
    MpdErrorApi,
    MpdErrorConnection,
    MpdErrorCommunication,
}

impl From<error::Error> for MpdError {
    fn from(value: error::Error) -> Self {
        match value {
            error::Error::Io(_) => MpdError::MpdErrorConnection,
            _ => MpdError::MpdErrorCommunication,
        }
    }
}

type MpdResult<T> = Result<T, MpdError>;
type MpdApi<T> = ConnectionResultWrapper<T>;

#[derive(Serialize, Elm, ElmDecode)]
enum ConnectionResultWrapper<T> {
    MpdError(MpdError),
    MpdOk(T),
}

fn result<T, F>(generator: F) -> Json<MpdApi<T>>
where
    F: Fn() -> MpdResult<T>,
{
    use ConnectionResultWrapper::*;
    Json(match generator() {
        Ok(v) => MpdOk(v),
        Err(e) => MpdError(e),
    })
}

#[derive(Serialize, Elm, ElmDecode)]
enum MpdSongShot {
    Default,
    Single,
    SingleRepeat,
    QueueRepeat,
}

impl From<Status> for MpdSongShot {
    fn from(value: Status) -> Self {
        use MpdSongShot::*;
        match (value.single, value.repeat) {
            (true, true) => SingleRepeat,
            (false, true) => QueueRepeat,
            (true, false) => Single,
            _ => Default,
        }
    }
}

#[derive(Serialize, Elm, ElmDecode)]
enum MpdState {
    Play,
    Pause,
    Stop,
}

impl From<State> for MpdState {
    fn from(value: State) -> Self {
        match value {
            State::Stop => MpdState::Stop,
            State::Play => MpdState::Play,
            State::Pause => MpdState::Pause,
        }
    }
}

#[derive(Serialize, Elm, ElmDecode)]
struct Song {
    file: String,

    name: Option<String>,
    title: Option<String>,
    artist: Option<String>,
    duration: Option<u64>,
    tags: Vec<(String, String)>,
    queue_id: Option<u32>,
}

impl Song {
    pub fn from_playlist_id(conn: &mut Mpd, id: Id) -> MpdResult<Song> {
        let raw = conn
            .playlistid(id)?
            .ok_or(MpdError::MpdErrorCommunication)?;

        Ok(Song::from_song(raw))
    }

    pub fn from_song(raw: mpd::Song) -> Song {
        Song {
            file: raw.file,
            name: raw.name,
            title: raw.title,
            artist: raw.artist,
            duration: raw.duration.map(|d| d.as_secs()),
            tags: raw.tags,
            queue_id: raw.place.map(|p| p.id.0),
        }
    }
}

#[derive(Serialize, Elm, ElmDecode)]
struct QueueSong {
    queue_id: u32,
    queue_pos: u32,
    song: Song,
}

impl QueueSong {
    pub fn from_place(conn: &mut Mpd, place: QueuePlace) -> MpdResult<QueueSong> {
        Ok(QueueSong {
            song: Song::from_playlist_id(conn, place.id)?,
            queue_id: place.id.0,
            queue_pos: place.pos,
        })
    }
}

#[derive(Serialize, Elm, ElmDecode)]
struct MpdStatus {
    state: MpdState,
    current_song: Option<QueueSong>,
    current_elapsed: Option<u64>,
    current_duration: Option<u64>,

    volume: Option<i8>,
    shot: MpdSongShot,
    random: bool,
    consume: bool,
}

impl MpdStatus {
    pub fn get(conn: &mut Mpd) -> MpdResult<MpdStatus> {
        let raw_status = conn.status()?;

        let current_song = match raw_status.song.map(|p| QueueSong::from_place(conn, p)) {
            Some(result) => result.map(Some),
            None => Ok(None),
        }?;

        Ok(MpdStatus {
            state: raw_status.state.into(),
            current_song,
            current_elapsed: raw_status.elapsed.map(|d| d.as_secs()),
            current_duration: raw_status.duration.map(|d| d.as_secs()),

            volume: if raw_status.volume <= -1 {
                None
            } else {
                Some(raw_status.volume)
            },
            random: raw_status.random,
            consume: raw_status.consume,
            shot: raw_status.into(),
        })
    }
}

type StatusResponse = MpdApi<MpdStatus>;
#[handler]
fn status() -> Json<StatusResponse> {
    result(|| {
        let mut conn = mpd()?;
        MpdStatus::get(&mut conn)
    })
}

#[derive(Serialize, Elm, ElmDecode)]
struct Queue(Vec<Song>);
type QueueResponse = MpdApi<Queue>;
#[handler]
fn queue() -> Json<QueueResponse> {
    result(|| {
        let mut conn = mpd()?;
        let raw_queue = conn.queue()?;

        Ok(Queue(raw_queue.into_iter().map(Song::from_song).collect()))
    })
}

#[handler]
fn playback_toggle() -> Json<StatusResponse> {
    result(|| {
        let mut conn = mpd()?;

        if conn.status()?.state == mpd::status::State::Stop {
            conn.play()?;
        } else {
            conn.toggle_pause()?;
        }

        MpdStatus::get(&mut conn)
    })
}

#[handler]
fn playback_prev() -> Json<StatusResponse> {
    result(|| {
        let mut conn = mpd()?;
        conn.prev()?;
        MpdStatus::get(&mut conn)
    })
}

#[handler]
fn playback_next() -> Json<StatusResponse> {
    result(|| {
        let mut conn = mpd()?;
        conn.next()?;
        MpdStatus::get(&mut conn)
    })
}

fn cr_elm<T: Write>(file: &mut T, name: &str, inner: &str) {
    file.write_fmt(format_args!(
        "type alias {name} = ConnectionResultWrapper {inner}\n\n"
    ))
    .expect("Failed to write to elm bindings");

    let name_decoder_name = format!(
        "{}{}",
        char::to_lowercase(name.chars().next().unwrap()),
        &name[1..]
    );

    let inner_decoder_name = format!(
        "{}{}",
        char::to_lowercase(inner.chars().next().unwrap()),
        &inner[1..]
    );

    file.write_fmt(format_args!(
        "{}Decoder : Json.Decode.Decoder {}\n",
        name_decoder_name, name
    ))
    .expect("Failed to write to elm bindings");

    file.write_fmt(format_args!(
        "{}Decoder = connectionResultWrapperDecoder {}Decoder\n\n",
        name_decoder_name, inner_decoder_name
    ))
    .expect("Failed to write to elm bindings");
}

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let path = Path::new("ui/Bindings.elm");
    let _ = remove_file(path);
    let mut file = File::create(path)?;

    elm_rs::export!("Bindings", &mut file, {
        decoders: [MpdError, MpdState, Song, QueueSong, MpdSongShot, MpdStatus, Queue]
    })
    .unwrap();

    file.write_all(ELM_CONNECTION_RESULT.as_bytes())
        .expect("Unnable to add connection result type");

    cr_elm(&mut file, "StatusResponse", "MpdStatus");
    cr_elm(&mut file, "QueueResponse", "Queue");

    let app = Route::new()
        .at("/status", get(status))
        .at("/queue", get(queue))
        .at("/playback/toggle", post(playback_toggle))
        .at("/playback/prev", post(playback_prev))
        .at("/playback/next", post(playback_next))
        .with(
            Cors::new()
                .allow_method(Method::GET)
                .allow_method(Method::POST),
        );

    Server::new(TcpListener::bind("0.0.0.0:3000"))
        .name("jukebox-api")
        .run(app)
        .await
}
