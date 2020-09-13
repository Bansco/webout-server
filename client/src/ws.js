import Convert from "ansi-to-html";

document.getElementById("form").addEventListener("submit", main);

function main(e) {
  e.preventDefault();

  const sessionInput = document.getElementById("session");
  const id = sessionInput.value;

  connect(id);
}

let socket;

function connect(fixedId) {
  const url = wsUrl(fixedId);
  if (socket) {
    socket.close();
  }
  socket = new WebSocket(url);

  const handleMessage = makeHandleMessage(new Convert({ stream: false }));

  socket.onopen = handleOpen;
  socket.onmessage = handleMessage;
  socket.onclose = handleClose;
  socket.onerror = handleError;

  const output = document.getElementById("output");
  output.innerHTML = "";

  removeAlert();
}

function wsUrl(givenId) {
  const protocol = location.protocol.match(/https/) ? "wss" : "ws";
  const id = givenId || location.pathname.replace("/c/", "").replace(/\//g, "");
  const host = givenId ? "localhost:9000" : location.host;

  return `${protocol}://${host}/api/cast/ws/${id}`;
}

function handleError(_error) {
  addAlert("There was an error with the socket connection.");

  window.scrollTo({ top: 0, behavior: "instant" });
}

function handleClose(_closeEvent) {
  addAlert("This session is closed!");

  window.scrollTo({ top: 0, behavior: "instant" });
}

function handleOpen(_openEvent) {
  addAlert("Connected to session!!!");

  setTimeout(removeAlert, 5 * 1000);
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

function addAlert(msg) {
  const alert = document.getElementById("alert");
  alert.innerHTML = msg;
  alert.classList.remove("hidden");
  alert.classList.add("alert");
}

function removeAlert() {
  const alert = document.getElementById("alert");
  alert.innerHTML = "";
  alert.classList.remove("alert");
  alert.classList.add("hidden");
}
