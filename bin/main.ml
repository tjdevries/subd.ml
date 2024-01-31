open Riot

let obs_ip_address = "192.168.5.118"
let obs_port = 4455

(* type Message.t += HelloWorld of string *)
type Message.t += RequestOBS of Subd.OpCodes.requests

module Echo_server = struct
  include Trail.Sock.Default

  type args = unit
  type state = int

  let init _args = `ok 0

  let handle_frame frame _conn state =
    Logger.info (fun f -> f "handling frame: %a" Trail.Frame.pp frame);
    match frame with
    | Text { fin; payload; compressed } ->
      (* Broadcast to everyone listening on "obs" the hello message *)
      let json = Yojson.Safe.from_string payload in
      let request = Subd.OpCodes.requests_of_yojson json in
      Subd.ChannelManager.broadcast (RequestOBS request) ~channel:"obs";
      (* Reply with the message *)
      `push
        ( [ Trail.Frame.Text { fin; payload = "ECHOED: " ^ payload; compressed }
          ]
        , state )
    | _ -> `push ([ frame ], state)
  ;;
end

let identify_response =
  let open Subd.OpCodes in
  let data =
    IdentifyData.
      { rpc_version = 1; authentication = None; event_subscriptions = Some 33 }
  in
  Identify.to_json data |> Yojson.Safe.to_string
;;

(* let set_scene_request = *)
(*   let open Subd.OpCodes in *)
(*   let data = SetCurrentProgramScene { sceneName = "Primary Stream - Zoomed" } in *)
(*   RequestData.t_of_request data |> Request.to_json |> Yojson.Safe.to_string *)
(* ;; *)

(* let set_scene_request_small = *)
(*   let open Subd.OpCodes in *)
(*   let data = SetCurrentProgramScene { sceneName = "Primary Stream - Small" } in *)
(*   RequestData.t_of_request data |> Request.to_json |> Yojson.Safe.to_string *)
(* ;; *)

(* let get_scene_list = *)
(*   let open Subd.OpCodes in *)
(*   let data = GetSceneItemList { sceneName = "Primary Stream - Small" } in *)
(*   RequestData.t_of_request data |> Request.to_json |> Yojson.Safe.to_string *)
(* ;; *)

(* let disable_camera_item = *)
(*   let open Subd.OpCodes in *)
(*   let data = *)
(*     SetSceneItemEnabled *)
(*       { sceneName = "Primary Stream - Small" *)
(*       ; sceneItemId = 2 *)
(*       ; sceneItemEnabled = true *)
(*       } *)
(*   in *)
(*   RequestData.t_of_request data |> Request.to_json |> Yojson.Safe.to_string *)
(* ;; *)

let data_to_string data =
  let open Subd.OpCodes in
  RequestData.t_of_request data |> Request.to_json |> Yojson.Safe.to_string
;;

let trail =
  let open Trail in
  let open Router in
  [ use (module Logger) Logger.(args ~level:Debug ())
  ; router
      [ get "/" (fun conn -> Conn.send_response `OK {%b|"hello world"|} conn)
      ; socket "/ws" (module Echo_server) ()
      ; scope "/api" [ get "/version" (Conn.send_response `OK {%b|"none"|}) ]
      ]
  ]
;;

[@@@warning "-8"]

let ( let* ) = Result.bind

let start_nvim_server () =
  let port = 8080 in
  let handler = Nomad.trail trail in
  let (Ok pid) = Nomad.start_link ~port ~handler () in
  Logger.info (fun f -> f "Listening on 0.0.0.0:%d" port);
  pid
;;

let start_obs_conn () =
  let send_request sock req =
    let request_frame = Trail.Frame.text ~fin:true req in
    let* sock = Blink.WebSocket.send [ request_frame ] sock in
    Logger.info (fun f -> f "sent Identify: %s" req);
    Ok sock
  in
  let handshake url =
    let url = Uri.of_string url in
    let* conn = Blink.connect url in
    let* sock = Blink.WebSocket.upgrade conn "/" in
    let* sock, [ frame ] = Blink.WebSocket.receive sock in
    Logger.debug (fun f -> f "recv Hello: %a" Trail.Frame.pp frame);
    let* sock = send_request sock identify_response in
    let* sock, [ frame ] = Blink.WebSocket.receive sock in
    Logger.debug (fun f -> f "recv Identified: %a" Trail.Frame.pp frame);
    Ok sock
  in
  let rec loop sock =
    match receive () with
    | RequestOBS request_data ->
      let* sock = send_request sock (data_to_string request_data) in
      loop sock
    | _ -> loop sock
  in
  let run_obs_conn () =
    let url = Fmt.str "http://%s:%d" obs_ip_address obs_port in
    let* sock = handshake url in
    (* let* sock = send_request sock set_scene_request_small in *)
    (* let* sock = send_request sock get_scene_list in *)
    (* let* sock = send_request sock disable_camera_item in *)
    loop sock
  in
  let pid =
    spawn (fun () ->
      match run_obs_conn () with
      | Ok _ -> Logger.info (fun f -> f "Completed obs connection")
      | Error _ ->
        Logger.info (fun f -> f "ERROR?!");
        failwith "ERROR")
  in
  Subd.ChannelManager.register pid ~channel:"obs";
  pid
;;

let () =
  Riot.run
  @@ fun () ->
  Logger.set_log_level (Some Info);
  let (Ok _) = Logger.start () in
  let (Ok manager) = Subd.ChannelManager.start_link () in
  let obs = start_obs_conn () in
  let nvim = start_nvim_server () in
  wait_pids [ manager; nvim; obs ]
;;
