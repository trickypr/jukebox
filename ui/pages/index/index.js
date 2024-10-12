import { Elm } from "../../Main.elm";

const app = Elm.Main.init({
  node: document.body,
});

console.log(app.ports);

const socket = new WebSocket("ws://localhost:3000/live");

socket.addEventListener("message", ({ data }) => {
  console.log("message", data);
  app.ports.serverUpdate.send(data);
});
