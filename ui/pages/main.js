import { Elm } from "../Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

let websocket_path = "ws://localhost:3000/";

if (import.meta.env.VITE_SERVER === "relative") {
  websocket_path = `wss://${window.location.host}/`;
} else if (import.meta.env.VITE_SERVER) {
  websocket_path = import.meta.env.VITE_SERVER;
}

const socket = new WebSocket(`${websocket_path}api/live`);

socket.addEventListener("message", ({ data }) => {
  console.log("message", data);
  app.ports.serverUpdate.send(data);
});
