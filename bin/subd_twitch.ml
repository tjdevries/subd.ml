open Riot

[@@@warning "-8"]

let () =
  Dotenv.export () |> ignore;
  let client_id = Sys.getenv "TWITCH_CLIENT_ID" in
  let token = Sys.getenv "TWITCH_OAUTH_TOKEN" in
  Riot.run
  @@ fun () ->
  Logger.set_log_level (Some Info);
  let (Ok _) = Logger.start () in
  let twitch =
    spawn (fun () ->
      let session = Subd.Twitch.connect ~client_id ~token () in
      match session with
      | Ok session -> Logger.info (fun f -> f "Connected to Twitch: %s" session)
      | Error `Connection_failed -> Fmt.failwith "Connection Failed"
      | Error `Closed -> Fmt.failwith "Closed"
      | Error `Connection_closed -> Fmt.failwith "Connection Closed"
      | Error `Eof -> Fmt.failwith "Eof"
      | Error `Excess_body_read -> Fmt.failwith "Excess Body Read"
      (* | Error `Exn of exn *)
      (* | Error `Invalid_uri of Uri.t *)
      (* | Error `Msg of string *)
      | Error `No_info -> Fmt.failwith "No info"
      | Error `Noop -> Fmt.failwith "Noop"
      | Error `Process_down -> Fmt.failwith "Process Down"
      | Error `Response_parsing_error -> Fmt.failwith "`Response_parsing_error"
      | Error `Timeout -> Fmt.failwith "Connection Failed"
      (* | Error `Tls_error of exn *)
      (* | Error `Unix_error of Unix.error *)
      (* | Error `Unknown_opcode of int *)
      | Error `Would_block -> Fmt.failwith "Would Block"
      | _ -> Fmt.failwith "Unknown error")
  in
  wait_pids [ twitch ]
;;
