open Riot

(* type Message.t += HelloWorld *)

let _s () =
  let open Subd.Result in
  let addr = Net.Addr.(tcp loopback 8080) in
  let* connection = Net.Tcp_stream.connect addr in
  let writer = Net.Tcp_stream.to_writer connection in
  let* () = IO.write_all writer ~buf:(Bytes.of_string "hello world") in
  Ok connection
;;

module Echo_server = struct
  include Trail.Sock.Default

  type args = unit
  type state = int

  let init _args = `ok 0

  let handle_frame frame _conn state =
    Logger.info (fun f -> f "handling frame: %a" Trail.Frame.pp frame);
    match frame with
    | Text { fin; payload; compressed } ->
      `push
        ( [ Trail.Frame.Text { fin; payload = "ECHOED: " ^ payload; compressed }
          ]
        , state )
    | _ -> `push ([ frame ], state)
  ;;
end

let trail =
  let open Trail in
  let open Router in
  [ use (module Logger) Logger.(args ~level:Debug ())
  ; router
      [ get "/" (fun conn -> Conn.send_response `OK {%b|"hello world"|} conn)
      ; socket "/ws" (module Echo_server) ()
      ; scope
          "/api"
          [ get "/version" (fun conn ->
              Conn.send_response `OK {%b|"none"|} conn)
          ; get "/version" (fun conn ->
              Conn.send_response `OK {%b|"none"|} conn)
          ; get "/version" (fun conn ->
              Conn.send_response `OK {%b|"none"|} conn)
          ]
      ]
  ]
;;

[@@@warning "-8"]

let () =
  Riot.run
  @@ fun () ->
  Logger.set_log_level (Some Debug);
  let (Ok _) = Logger.start () in
  sleep 0.1;
  let port = 8080 in
  let handler = Nomad.trail trail in
  let (Ok pid) = Nomad.start_link ~port ~handler () in
  Logger.info (fun f -> f "Listening on 0.0.0.0:%d" port);
  wait_pids [ pid ]
;;
