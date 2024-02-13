open Riot

open Logger.Make (struct
    let namespace = [ "subd" ]
  end)

module Broadcaster = struct
  let start () = Subd.ChannelManager.start_link ()
end

module TwitchApp = struct
  let start () =
    let client_id = Sys.getenv "TWITCH_CLIENT_ID" in
    let token = Sys.getenv "TWITCH_OAUTH_TOKEN" in
    Logger.set_log_level (Some Info);
    Ok
      (spawn (fun () ->
         warn (fun f -> f "Beginning twitch 2...");
         let pids = Subd.Twitch.connect ~client_id ~token () in
         wait_pids pids))
  ;;
end

let () =
  Dotenv.export () |> ignore;
  Riot.start
    ~apps:[ (module Logger); (module Broadcaster); (module TwitchApp) ]
    ()
;;
