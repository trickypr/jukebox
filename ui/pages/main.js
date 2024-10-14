import { Elm } from "../Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

const socket = new WebSocket(
  `ws://${import.meta.env.SERVER ?? "localhost:3000/"}api/live`,
);

socket.addEventListener("message", ({ data }) => {
  console.log("message", data);
  app.ports.serverUpdate.send(data);
});
