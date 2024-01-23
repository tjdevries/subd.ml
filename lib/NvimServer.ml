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
