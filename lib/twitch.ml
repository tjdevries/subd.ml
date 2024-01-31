open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let ( let* ) = Result.bind

type session_payload = { id : string }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type session_welcome = { session : session_payload } [@@deriving yojson]
type payload = { payload : session_welcome } [@@deriving yojson]

let connect () =
  let url = "https://eventsub.wss.twitch.tv/ws" in
  let url = Uri.of_string url in
  let* conn = Blink.connect url in
  let* sock = Blink.WebSocket.upgrade conn "/ws" in
  let* sock, frames = Blink.WebSocket.receive sock in
  let* sock =
    match frames with
    | [ Ping ] -> Blink.WebSocket.send [ Pong ] sock
    | _ -> failwith "Expected a ping"
  in
  let* _, frames = Blink.WebSocket.receive sock in
  let session_id =
    match frames with
    | [ Text { payload; _ } ] ->
      let json = Yojson.Safe.from_string payload in
      let session_welcome = payload_of_yojson json in
      session_welcome.payload.session.id
    | frames ->
      List.iter (Fmt.pr "Weird frames: %a@." Trail.Frame.pp) frames;
      assert false
  in
  Ok session_id
;;
