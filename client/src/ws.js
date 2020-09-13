import Convert from "ansi-to-html";

document.getElementById("form").addEventListener("submit", main);

function main(e) {
  e.preventDefault();

  const sessionInput = document.getElementById("session");
  const id = sessionInput.value;

  connect(id);
}

function connect(fixedId) {
  const url = wsUrl(fixedId);
  const socket = new WebSocket(url);

  const convert = new Convert({ stream: false });

  socket.onmessage = makeHandleMessage(convert);
  socket.onclose = handleClose;
  socket.onerror = handleError;
}

function wsUrl(givenId) {
  const protocol = location.protocol.match(/https/) ? "wss" : "ws";
  const id = givenId || location.pathname.replace("/c/", "").replace(/\//g, "");
  const host = givenId ? "localhost:9000" : location.host;

  return `${protocol}://${host}/api/cast/ws/${id}`;
}

function handleError(_error) {
  const alert = document.getElementById("alert");
  alert.classList.add("alert");
  alert.innerHTML = "There was an error with the socket connection.";
  alert.classList.remove("hidden");

  window.scrollTo({ top: 0, behavior: "instant" });
}

function handleClose(_closeEvent) {
  const alert = document.getElementById("alert");
  alert.classList.add("alert");
  alert.innerHTML = "This channel is closed!";
  alert.classList.remove("hidden");

  window.scrollTo({ top: 0, behavior: "instant" });
}

const makeHandleMessage = (convert) => {
  function handleMessage(msgEvent) {
    const output = document.getElementById("output");
    const newLine = document.createElement("span");

    newLine.innerHTML = convert.toHtml(msgEvent.data);
    output.appendChild(newLine);
    window.scrollTo({ top: output.clientHeight, behavior: "instant" });
  }

  return handleMessage;
};
