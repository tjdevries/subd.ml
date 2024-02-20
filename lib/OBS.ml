let ( let* ) = Result.bind

open Riot

(* let obs_ip_address = "192.168.5.115" *)
let obs_ip_address = "192.168.4.121"
let obs_port = 4455

let identify_response =
  let open OpCodes in
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
  let open OpCodes in
  RequestData.t_of_request data |> Request.to_json |> Yojson.Safe.to_string
;;

[@@@warning "-8"]

module App = struct
  let start () =
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
      | OpCodes.RequestOBS request_data ->
        let* sock = send_request sock (data_to_string request_data) in
        loop sock
      | Twitch.GiftedSubscription _ ->
        let* sock =
          send_request
            sock
            (data_to_string
               (OpCodes.SetCurrentProgramScene { sceneName = "Camera - Base" }))
        in
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
    ChannelManager.register pid ~channel:"obs";
    Ok pid
  ;;
end
