var ip = "ws://192.168.5.118:4455";

var socket = new WebSocket(ip);

socket.onopen = function (event) {
  console.log("open", event);
};

socket.onerror = function (event) {
  console.log("error", event);
};

socket.onclose = function (event) {
  console.log("close", event);
};
