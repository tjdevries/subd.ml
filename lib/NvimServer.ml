open Riot
open Piaf

let get_event str =
  let parsed = Yojson.Safe.from_string str in
  (Nvim.nvim_message_of_yojson parsed).event
;;

let connection_handler other_wsd { Server.request; _ } =
  Response.Upgrade.websocket request ~f:(fun wsd ->
    let frames = Ws.Descriptor.messages wsd in
    Format.printf "Got frames\n%!";
    Stream.iter
      ~f:(fun (_opcode, ({ IOVec.buffer; off; len } as frame)) ->
        let substr = Bigstringaf.substring ~off ~len buffer in
        Format.printf "Inside stream... %s@." substr;
        let event = get_event substr in
        match event with
        | "SetScene" ->
          Format.printf "Got SetScene event\n%!";
          Ws.Descriptor.send_string
            other_wsd
            {| {
                "op": 6,
                "d": {
                  "requestType": "SetCurrentProgramScene",
                  "requestId": "f819dcf0-89cc-11eb-8f0e-382c4ac93b9c",
                  "requestData": {
                    "sceneName": "Primary Stream - Zoomed"
                  }
                }
            } |};
          Format.printf "Sent SetScene event\n%!"
        | _ -> Ws.Descriptor.send_iovec wsd frame)
      frames)
  |> Result.get_ok
;;

let make port env sw wsd =
  Format.printf "Listening on port %d\n%!" port;
  let config = Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  let server = Server.create ~config (connection_handler wsd) in
  let _command = Server.Command.start ~sw env server in
  ()
;;

module NeovimListener = struct
  include Trail.Sock.Default

  type args = unit
  type state = int

  let init _args = `ok 0

  let handle_frame frame _conn state =
    Logger.info (fun f -> f "handling frame: %a" Trail.Frame.pp frame);
    match frame with
    | Text { fin; payload; compressed } ->
      let json = Yojson.Safe.from_string payload in
      let request = OpCodes.requests_of_yojson json in
      ChannelManager.broadcast (OpCodes.RequestOBS request) ~channel:"obs";
      `push ([ Trail.Frame.Text { fin; payload = "[true]"; compressed } ], state)
    | _ -> `push ([ frame ], state)
  ;;
end

let trail =
  let open Trail in
  let open Router in
  [ use (module Logger) Logger.(args ~level:Debug ())
  ; router [ socket "/ws" (module NeovimListener) () ]
  ]
;;

let ( let* ) = Result.bind

module App = struct
  let start () =
    let port = 8080 in
    let handler = Nomad.trail trail in
    let* pid = Nomad.start_link ~port ~handler () in
    Logger.info (fun f -> f "Listening on 0.0.0.0:%d" port);
    Ok pid
  ;;
end
