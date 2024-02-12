open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let ( let* ) = Result.bind

type session_payload = { id : string }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type session_welcome = { session : session_payload }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type payload = { payload : session_welcome }
[@@deriving yojson] [@@yojson.allow_extra_fields]

let next_message sock () =
  let rec loop sock =
    let* sock, frames = Blink.WebSocket.receive sock in
    List.iter (fun frame -> Fmt.pr "RECEIVED: %a@." Trail.Frame.pp frame) frames;
    match frames with
    | Ping :: [] ->
      let* sock = Blink.WebSocket.send [ Pong ] sock in
      loop sock
    | Ping :: tl ->
      let* sock = Blink.WebSocket.send [ Pong ] sock in
      Ok (sock, tl)
    | msg -> Ok (sock, msg)
  in
  loop sock
;;

module Twitch = struct
  let base_url = "https://api.twitch.tv/"
  let get_conn () = Blink.connect (Uri.of_string base_url)

  let make_headers ~client_id ~token ?(headers = []) () =
    let headers =
      [ "Client-Id", client_id
      ; "Authorization", Fmt.str "Bearer %s" token
      ; "Accept", "application/json"
      ]
      @ headers
    in
    Http.Header.of_list headers
  ;;

  let make_request api ~meth ~headers ~path ?(body = "") () =
    let req = Http.Request.make ~meth ~headers path in
    let body = Riot.Bytestring.of_string body in
    let* conn = Blink.request api req ~body () in
    Requests.stream_until_done conn
  ;;
end

let show_subscriptions ~conn ~client_id ~token () =
  Fmt.pr "SHOWING SUBSCRIPTIONS@.";
  (* curl -X GET 'https://api.twitch.tv/helix/eventsub/subscriptions' \ *)
  (* -H 'Authorization: Bearer 2gbdx6oar67tqtcmt49t3wpcgycthx' \ *)
  (* -H 'Client-Id: wbmytr93xzw8zbg0p1izqyzzc5mbiz' *)
  let headers = Twitch.make_headers ~client_id ~token () in
  Twitch.make_request
    conn
    ~meth:`GET
    ~headers
    ~path:"/helix/eventsub/subscriptions"
    ()
;;

let connect ~client_id ~token () =
  let broadcaster_id = "114257969" in
  let* conn = Twitch.get_conn () in
  let* sock = Blink.WebSocket.upgrade conn "/ws" in
  let* _sock, msg = next_message sock () in
  let session_id =
    match msg with
    | Text { payload; _ } :: _ ->
      Fmt.pr "RECEIVED TEXT: %s@." payload;
      let json = Yojson.Safe.from_string payload in
      let session_welcome = payload_of_yojson json in
      session_welcome.payload.session.id
    | frames ->
      List.iter (Fmt.pr "Weird frames: %a@." Trail.Frame.pp) frames;
      assert false
  in
  let* _ = Blink.WebSocket.send [ Pong ] sock in
  (* Make a post to subscriptions to subscribe to things (per sub) *)
  (* btw you should sub to me PogChamp *)
  Fmt.pr "... Requesting subscriptions ...@.";
  let* conn, subscriptions = show_subscriptions ~conn ~client_id ~token () in
  Fmt.pr "=> SUBSCRIPTIONS: %a@." Requests.pp_request_result subscriptions;
  let headers = Twitch.make_headers ~client_id ~token () in
  let body =
    Fmt.str
      {|
  {
    "type": "channel.goal.begin",
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
  let* conn, reply =
    Twitch.make_request
      conn
      ~meth:`POST
      ~headers
      ~path:"/helix/eventsub/subscriptions"
      ~body
      ()
  in
  Fmt.pr "<== SUBSCRIBE: %a@." Requests.pp_request_result reply;
  let* _conn, subscriptions = show_subscriptions ~conn ~client_id ~token () in
  Fmt.pr "=> SUBSCRIPTIONS: %a@." Requests.pp_request_result subscriptions;
  Ok session_id
;;
