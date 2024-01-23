(* HACKD! Part 2 - eledctrirc booooogalooo*)
let ip_address = "http://192.168.5.118:4455"

open Eio.Std
open Piaf

let rec stdin_loop ~stdin buf wsd =
  let line = Eio.Buf_read.line buf in
  traceln "< %s" line;
  match line with
  | "exit" -> Ws.Descriptor.close wsd
  | "ping" ->
    let application_data =
      IOVec.make ~off:0 ~len:5 (Bigstringaf.of_string ~off:0 ~len:5 "hello")
    in
    Ws.Descriptor.send_ping ~application_data wsd;
    stdin_loop ~stdin buf wsd
  | line ->
    Ws.Descriptor.send_string wsd line;
    stdin_loop ~stdin buf wsd
;;

let identify_response =
  let open Subd.OpCodes in
  let data =
    IdentifyData.
      { rpc_version = 1; authentication = None; event_subscriptions = None }
  in
  Identify.to_json data |> Yojson.Safe.to_string
;;

let handle_message wsd str =
  let s = Yojson.Safe.from_string str in
  let parsed = Subd.OpCodes.op_only_of_yojson s in
  match parsed.op with
  | 0 ->
    Format.printf ">> Got Hello @.";
    Ws.Descriptor.send_string wsd identify_response;
    Format.printf "<< Sending.... %s@." identify_response
    (* Ws.Descriptor.send_string *)
    (*   wsd *)
    (*   {| { *)
    (*             "op": 6, *)
    (*             "d": { *)
    (*               "requestType": "SetCurrentProgramScene", *)
    (*               "requestId": "f819dcf0-89cc-11eb-8f0e-382c4ac93b9c", *)
    (*               "requestData": { *)
    (*                 "sceneName": "Primary Stream - Zoomed" *)
    (*               } *)
    (*             } *)
    (*         } |} *)
  | 1 -> Format.printf "Identify @."
  | 2 -> Format.printf "Identified @."
  | _ -> Format.printf "Unknown op code %d %s @." parsed.op str
;;

let request ~env ~sw host =
  let open Subd.Result in
  let* client = Client.create env ~sw (Uri.of_string host) in
  let+ wsd = Client.ws_upgrade client "/" in
  Fiber.all
    [ (fun () ->
        let stdin = Eio.Stdenv.stdin env in
        let buf =
          Eio.Buf_read.of_flow stdin ~initial_size:100 ~max_size:1_000
        in
        stdin_loop ~stdin buf wsd;
        Client.shutdown client)
    ; (fun () ->
        Stream.iter
          ~f:(fun (_opcode, { IOVec.buffer; off; len }) ->
            let substr = Bigstringaf.substring ~off ~len buffer in
            Format.printf ">> %s@." substr;
            handle_message wsd substr)
          (Ws.Descriptor.messages wsd))
    ; (fun () -> Subd.NvimServer.make 8080 env sw wsd)
    ]
;;

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
;;

let () =
  setup_log (Some Logs.Debug);
  let host = ref None in
  Arg.parse
    []
    (fun host_argument -> host := Some host_argument)
    "eio_get.exe HOST";
  let host =
    match !host with
    | None -> ip_address
    | Some host -> host
  in
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      match request ~sw ~env host with
      | Ok () -> ()
      | Error e -> failwith (Piaf.Error.to_string e)))
;;
