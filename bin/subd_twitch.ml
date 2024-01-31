open Riot

[@@@warning "-8"]

let () =
  Riot.run
  @@ fun () ->
  Logger.set_log_level (Some Info);
  let (Ok _) = Logger.start () in
  let twitch =
    spawn (fun () ->
      let session = Subd.Twitch.connect () in
      match session with
      | Ok session -> Logger.info (fun f -> f "Connected to Twitch: %s" session)
      | _ -> failwith "OH NO")
  in
  wait_pids [ twitch ]
;;
