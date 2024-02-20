open Riot

type Message.t += GiftedSubscription of string

module Logger = Logger.Make (struct
    let namespace = [ "subd"; "twitch" ]
  end)

open Logger

let ( let* ) = Result.bind
let broadcaster_id = "114257969"

module Twitch = struct
  let base_url = "https://api.twitch.tv/helix"
  let get_conn () = Blink.connect (Uri.of_string base_url)

  let make_headers ~client_id ~token ?(headers = []) () =
    let headers =
      [ "Client-Id", client_id
      ; "Authorization", Fmt.str "Bearer %s" token
      ; "Content-Type", "application/json"
      ]
      @ headers
    in
    Http.Header.of_list headers
  ;;

  let make_request conn ~meth ~headers ~path ?(body = "") () =
    let req = Http.Request.make ~meth ~headers path in
    let body = Riot.Bytestring.of_string body in
    info (fun f -> f "Sending request...");
    let* conn = Blink.request conn req ~body () in
    info (fun f -> f "Streaming result...");
    let* res = Requests.stream_until_done conn in
    Ok (snd res)
  ;;
end

let show_subscriptions ~client_id ~token () =
  let* conn = Twitch.get_conn () in
  let headers = Twitch.make_headers ~client_id ~token () in
  Twitch.make_request
    conn
    ~meth:`GET
    ~headers
    ~path:"/helix/eventsub/subscriptions"
    ()
;;

let post_subscription body ~client_id ~token () =
  let* conn = Twitch.get_conn () in
  let headers = Twitch.make_headers ~client_id ~token () in
  let* reply =
    Twitch.make_request
      conn
      ~meth:`POST
      ~headers
      ~path:"/helix/eventsub/subscriptions"
      ~body
      ()
  in
  Ok reply
;;

let subscribe_to_channel_updates
  ~client_id
  ~token
  ~broadcaster_id
  ~session_id
  ()
  =
  let body =
    Fmt.str
      {|
  {
    "type": "channel.update",
    "version": 1,
    "condition": {
      "broadcaster_user_id": "%s"
    },
    "transport": {
      "method": "websocket",
      "session_id": "%s"
    }
  } |}
      broadcaster_id
      session_id
  in
  post_subscription body ~client_id ~token ()
;;

let subscribe_to_channel_chat ~client_id ~token ~broadcaster_id ~session_id () =
  let body =
    Fmt.str
      {|
  {
    "type": "channel.chat.message",
    "version": 1,
    "condition": {
      "broadcaster_user_id": "%s",
      "user_id": "%s"
    },
    "transport": {
      "method": "websocket",
      "session_id": "%s"
    }
  } |}
      broadcaster_id
      broadcaster_id
      session_id
  in
  post_subscription body ~client_id ~token ()
;;

let subscribe_to_channel_subscription_gift
  ~client_id
  ~token
  ~broadcaster_id
  ~session_id
  ()
  =
  let body =
    Fmt.str
      {|
  {
    "type": "channel.subscription.gift",
    "version": "1",
    "condition": {
        "broadcaster_user_id": "%s"
    },
    "transport": {
      "method": "websocket",
      "session_id": "%s"
    }
  } |}
      broadcaster_id
      session_id
  in
  post_subscription body ~client_id ~token ()
;;

let twitch_eventsub_url = "https://eventsub.wss.twitch.tv/ws"

type Message.t += Ws of (Pid.t * Trail.Frame.t)
type Message.t += SendFrame of Trail.Frame.t

let broadcast_frame frame =
  ChannelManager.broadcast (Ws frame) ~channel:"twitch-websocket"
;;

let websocket_handler () =
  info (fun f -> f "Starting websocket handler");
  let handle_message sock =
    match receive ~after:1000L () with
    | SendFrame Pong ->
      info (fun f -> f "-> Pong");
      Blink.WebSocket.send [ Pong ] sock
    | SendFrame frame ->
      info (fun f -> f "-> Sending frame response");
      Blink.WebSocket.send [ frame ] sock
    | _ -> Ok sock
    | exception Receive_timeout -> Ok sock
  in
  let rec loop sock =
    let* sock = handle_message sock in
    let* sock, frames = Blink.WebSocket.receive sock in
    List.iter (fun frame -> broadcast_frame (self (), frame)) frames;
    loop sock
  in
  let url = Uri.of_string twitch_eventsub_url in
  let* eventsub_conn = Blink.connect url in
  info (fun f -> f "Connected to twitch http endpoint");
  let* sock = Blink.WebSocket.upgrade eventsub_conn "/ws" in
  info (fun f -> f "Connected to twitch websocket");
  loop sock
;;

let handle_keepalive () =
  info (fun f -> f "=> handling keepalive");
  (* TODO: At some point we should try and reconnect if we don't get one of these *)
  Ok ()
;;

let handle_session_welcome
  (session : TwitchMessages.session)
  session_id
  ~conn
  ~client_id
  ~token
  =
  info (fun f -> f "=> setting session_id: %s" session.id);
  session_id := Some session.id;
  let* _ = show_subscriptions ~client_id ~token () in
  let* _ =
    subscribe_to_channel_updates
      ~client_id
      ~token
      ~session_id:session.id
      ~broadcaster_id
      ()
  in
  let* reply =
    subscribe_to_channel_chat
      ~client_id
      ~token
      ~session_id:session.id
      ~broadcaster_id
      ()
  in
  let* _ =
    subscribe_to_channel_subscription_gift
      ~client_id
      ~token
      ~session_id:session.id
      ~broadcaster_id
      ()
  in
  match reply.status with
  | `Bad_request | `Code 400 ->
    error (fun f -> f "Failed to subscribe to channel.update@.");
    Ok ()
  | _ ->
    sleep 1.0;
    let* _ = show_subscriptions ~client_id ~token () in
    let _ = conn in
    Ok ()
;;

let twitch_handler ~client_id ~token () =
  let* conn = Twitch.get_conn () in
  let session_id = ref None in
  let rec loop () =
    match receive () with
    | Ws (pid, Trail.Frame.Ping) ->
      info (fun f -> f "<- Ping");
      send pid (SendFrame Trail.Frame.Pong);
      loop ()
    | Ws (_, Trail.Frame.Text { payload; _ }) ->
      let open TwitchMessages in
      trace (fun f -> f "Parsing twitch message...: %s@." payload);
      let message = get_twitch_message payload in
      info (fun f -> f "Handling twitch message...");
      let* () =
        match message with
        | Ok (SessionWelcome session) ->
          info (fun f -> f "Session welcome: %s" session.session.id);
          handle_session_welcome
            session.session
            session_id
            ~conn
            ~client_id
            ~token
        | Ok KeepAlive -> handle_keepalive ()
        | Ok (ChannelUpdate update) ->
          info (fun f -> f "=> Got an update: %s" update.event.title);
          Ok ()
        | Ok (ChannelSubscriptionGift gift) ->
          info (fun f -> f "=> Got a gifted sub from: %s" gift.event.user_name);
          (* TODO: What should the channels be?... it seems weird to send
             to OBS directly? *)
          ChannelManager.broadcast
            (GiftedSubscription gift.event.user_name)
            ~channel:"obs";
          Ok ()
        | Error err ->
          info (fun f -> f "=> unhandled: %s \n\n%a" payload Serde.pp_err err);
          Ok ()
      in
      loop ()
    | _ -> loop ()
  in
  loop ()
;;

let connect ~client_id ~token () =
  sleep 0.5;
  info (fun f -> f "Starting connections");
  let _ = client_id in
  let _ = token in
  let websocket_pid =
    spawn_link (fun () ->
      match websocket_handler () with
      | Ok () -> ()
      | _ -> failwith "Twitch Websocket Died")
  in
  let handler_pid =
    spawn_link (fun () ->
      match twitch_handler ~client_id ~token () with
      | Ok () -> ()
      | _ -> failwith "Twitch Handler Died")
  in
  ChannelManager.register handler_pid ~channel:"twitch-websocket";
  [ websocket_pid ]
;;
