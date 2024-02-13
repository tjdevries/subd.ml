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

let () =
  let open Alcotest in
  run
    "twitch serde"
    [ ( "deserialization"
      , [ test_case "session_welcome" `Quick test_session_welcome ] )
    ]
;;
