let session_welcome_message =
  {|{"metadata":{"message_id":"934ce414-53da-4405-9e12-1dbc64c91102","message_type":"session_welcome","message_timestamp":"2024-02-19T21:26:28.329811482Z"},"payload":{"session":{"id":"AgoQPe70BAD9Si24I5HwwNoKpxIGY2VsbC1i","status":"connected","connected_at":"2024-02-19T21:26:28.320764599Z","keepalive_timeout_seconds":10,"reconnect_url":null}}}|}
;;

let message =
  {|
{
  "metadata": {
    "message_id": "79802166-3088-482a-8033-9c8287dc51f4",
    "message_type": "session_welcome",
    "message_timestamp": "2024-02-12T19:07:34.616439755Z"
  },
  "payload": {
    "session": {
      "id": "AgoQ99qmNLJVTROlJQRa7p06MxIGY2VsbC1i",
      "status": "connected",
      "connected_at": "2024-02-12T19:07:34.606893659Z",
      "keepalive_timeout_seconds": 10,
      "reconnect_url": null
    }
  }
} |}
;;

let notification_message =
  {|
{
  "metadata": {
    "message_id": "GkeBfZIW1EHY6KCu1k0n7YoHdUYbh1HigP3S_IwGjzk=",
    "message_type": "notification",
    "message_timestamp": "2024-02-16T18:03:15.174276381Z",
    "subscription_type": "channel.update",
    "subscription_version": "1"
  },
  "payload": {
    "subscription": {
      "id": "8f71fb49-689d-49c9-bc2d-57c5f32d4760",
      "status": "enabled",
      "type": "channel.update",
      "version": "1",
      "condition": {
        "broadcaster_user_id": "114257969"
      },
      "transport": {
        "method": "websocket",
        "session_id": "AgoQPH_JU0fRSymh8qdfpEVbehIGY2VsbC1i"
      },
      "created_at": "2024-02-16T18:03:08.895062585Z",
      "cost": 0
    },
    "event": {
      "broadcaster_user_id": "114257969",
      "broadcaster_user_login": "teej_dv",
      "broadcaster_user_name": "teej_dv",
      "title": "OCAML MY CAML | !today",
      "language": "en",
      "category_id": "1469308723",
      "category_name": "Software and Game De velopment",
      "is_mature": false
    }
  }
}
  |}
;;

let gift_subscription_message =
  {|
{"metadata":{"message_id":"qBPv75PVVulbXx-N1uKw4OXg_ICg72HGTA3R_OLbaRU=","message_type":"notification","message_timestamp":"2024-02-20T16:24:36.333794712Z","subscription_type":"channel.subscription.gift","subscription_version":"1"},"payload":{"subscription":{"id":"a57deb2c-4b28-4f36-96f4-3963017d588e","status":"enabled","type":"channel.subscription.gift","version":"1","condition":{"broadcaster_user_id":"114257969"},"transport":{"method":"websocket","session_id":"AgoQitwxSH6RTWOXK9OGkSI6pRIGY2VsbC1i"},"created_at":"2024-02-20T16:24:17.458103767Z","cost":0},"event":{"user_id":"931884170","user_login":"frenchsardo","user_name":"FrenchSardo","broadcaster_user_id":"114257969","broadcaster_user_login":"teej_dv","broadcaster_user_name":"teej_dv","tier":"1000","total":1,"cumulative_total":39,"is_anonymous":false}}}
|}
;;

let test_session_welcome () =
  let parsed = Subd.TwitchMessages.get_twitch_message message in
  match parsed with
  | Ok (SessionWelcome { session }) ->
    Alcotest.(
      check
        string
        "Matching IDs"
        "AgoQ99qmNLJVTROlJQRa7p06MxIGY2VsbC1i"
        session.id)
  | Ok _ -> Alcotest.fail "Got the wrong message type"
  | Error err -> Alcotest.failf "Oh no, failed to parse: %a" Serde.pp_err err
;;

let test_parse_notification_message () =
  let parsed = Subd.TwitchMessages.get_twitch_message notification_message in
  match parsed with
  | Ok (ChannelUpdate { subscription; _ }) ->
    Alcotest.(
      check
        string
        "Matching Broadcaster ID"
        "8f71fb49-689d-49c9-bc2d-57c5f32d4760"
        subscription.id)
  | Ok _ -> Alcotest.fail "Got the wrong message type"
  | Error err -> Alcotest.failf "Oh no, failed to parse: %a" Serde.pp_err err
;;

let test_session_welcome_min () =
  let parsed = Subd.TwitchMessages.get_twitch_message session_welcome_message in
  match parsed with
  | Ok (SessionWelcome { session }) ->
    Alcotest.(
      check
        string
        "Matching IDs"
        "AgoQPe70BAD9Si24I5HwwNoKpxIGY2VsbC1i"
        session.id)
  | Ok _ -> Alcotest.fail "Got the wrong message type"
  | Error err -> Alcotest.failf "Oh no, failed to parse: %a" Serde.pp_err err
;;

let test_gift_subscription () =
  let parsed =
    Subd.TwitchMessages.get_twitch_message gift_subscription_message
  in
  match parsed with
  | Ok (ChannelSubscriptionGift { event }) ->
    Alcotest.(check string "Matching User Name" "FrenchSardo" event.user_name);
    Alcotest.(check string "Matching User Login" "frenchsardo" event.user_login)
  | Ok _ -> Alcotest.fail "Got the wrong message type"
  | Error err -> Alcotest.failf "Oh no, failed to parse: %a" Serde.pp_err err
;;

let () =
  let open Alcotest in
  run
    "twitch serde"
    [ ( "deserialization"
      , [ test_case "session_welcome:min" `Quick test_session_welcome_min
        ; test_case "session_welcome" `Quick test_session_welcome
        ; test_case "channel.update" `Quick test_parse_notification_message
        ; test_case "channel.subscription.gift" `Quick test_gift_subscription
        ] )
    ]
;;
