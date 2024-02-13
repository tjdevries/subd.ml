(*   "metadata": { *)
(*     "message_id": "96a3f3b5-5dec-4eed-908e-e11ee657416c", *)
(*     "message_type": "session_welcome", *)
(*     "message_timestamp": "2023-07-19T14:56:51.634234626Z" *)
(*   }, *)
type twitch_type_metadata =
  { message_id : string
  ; message_type : string
  ; message_timestamp : string
  }

(* { *)
(*   "metadata": { *)
(*     "message_id": "96a3f3b5-5dec-4eed-908e-e11ee657416c", *)
(*     "message_type": "session_welcome", *)
(*     "message_timestamp": "2023-07-19T14:56:51.634234626Z" *)
(*   }, *)
(*   "payload": { *)
(*     "session": { *)
(*       "id": "AQoQILE98gtqShGmLD7AM6yJThAB", *)
(*       "status": "connected", *)
(*       "connected_at": "2023-07-19T14:56:51.616329898Z", *)
(*       "keepalive_timeout_seconds": 10, *)
(*       "reconnect_url": null *)
(*     } *)
(*   } *)
(* } *)

type twitch_type_session =
  { id : string
  ; status : string
  ; connected_at : string
  ; keepalive_timeout_seconds : int
  ; reconnect_url : string option
  }

(* { *)
(*     "metadata": { *)
(*         "message_id": "84c1e79a-2a4b-4c13-ba0b-4312293e9308", *)
(*         "message_type": "session_keepalive", *)
(*         "message_timestamp": "2023-07-19T10:11:12.634234626Z" *)
(*     }, *)
(*     "payload": {} *)
(* } *)
(* type twitch_message_keepalive = unit *)

(* { *)
(*     "subscription": { *)
(*         "id": "f1c2a387-161a-49f9-a165-0f21d7a4e1c4", *)
(*         "type": "channel.update", *)
(*         "version": "2", *)
(*         "status": "enabled", *)
(*         "cost": 0, *)
(*         "condition": { *)
(*            "broadcaster_user_id": "1337" *)
(*         }, *)
(*          "transport": { *)
(*             "method": "webhook", *)
(*             "callback": "https://example.com/webhooks/callback" *)
(*         }, *)
(*         "created_at": "2023-06-29T17:20:33.860897266Z" *)
(*     }, *)
(*     "event": { *)
(*         "broadcaster_user_id": "1337", *)
(*         "broadcaster_user_login": "cool_user", *)
(*         "broadcaster_user_name": "Cool_User", *)
(*         "title": "Best Stream Ever", *)
(*         "language": "en", *)
(*         "category_id": "12453", *)
(*         "category_name": "Grand Theft Auto", *)
(*         "content_classification_labels": [ "MatureGame" ] *)
(*     } *)
(* } *)

(* Name     Type        Description *)
(* id       string      Your client ID. *)
(* type     string      The notification’s subscription type. *)
(* version  string      The version of the subscription. *)
(* status   string      The status of the subscription. *)
(* cost     integer     How much the subscription counts against your limit. See Subscription Limits for more information. *)
(* condition             condition  Subscription-specific parameters. *)
(* created_at           string      The time the notification was created. *)

type twitch_type_subscription =
  { id : string
  ; type_ : string [@key "type"]
  ; version : string
  ; status : string
  ; cost : int
  ; (* condition: *)
    created_at : string
  }

(* Name     Type        Description *)
(* broadcaster_user_id  string      The broadcaster’s user ID. *)
(* broadcaster_user_login           string      The broadcaster’s user login. *)
(* broadcaster_user_name            string      The broadcaster’s user display name. *)
(* title    string      The channel’s stream title. *)
(* language string      The channel’s broadcast language. *)
(* category_id          string      The channel’s category ID. *)
(* category_name        string      The category name. *)
(* content_classification_labels    string[]    Array of content classification label IDs currently applied on the Channel. To retrieve a list of all possible IDs, use the Get Content Classification Labels API endpoint. *)
type twitch_type_event =
  { broadcaster_user_id : string
  ; broadcaster_user_login : string
  ; broadcaster_user_name : string
  ; title : string
  ; language : string
  ; category_id : string
  ; category_name : string (* ; content_classification_labels : string list *)
  }

type twitch_message =
  | SessionWelcome of { session : twitch_type_session }
  | KeepAlive of unit
  | ChannelUpdate of
      { subscription : twitch_type_subscription
      ; event : twitch_type_event
      }

let with_payload payload_de =
  Serde.De.(
    deserializer
    @@ fun ctx ->
    record ctx "payload" 1 @@ fun ctx -> field ctx "session" (d payload_de))
;;

let deserialize_twitch_type_session =
  let open Serde_json in
  Serde.De.(
    deserializer
    @@ fun ctx ->
    record ctx "session" 3 (fun ctx ->
      let* id = field ctx "id" string in
      let* status = field ctx "status" string in
      let* connected_at = field ctx "connected_at" string in
      let* keepalive_timeout_seconds =
        field ctx "keepalive_timeout_seconds" int
      in
      Ok
        { id
        ; status
        ; connected_at
        ; keepalive_timeout_seconds
        ; reconnect_url = None
        }))
;;

let deserialize_twitch_type_metadata =
  let open Serde_json in
  Serde.De.(
    deserializer
    @@ fun ctx ->
    record ctx "metadata" 3 (fun ctx ->
      let* message_id = field ctx "message_id" string in
      let* message_type = field ctx "message_type" string in
      let* message_timestamp = field ctx "message_timestamp" string in
      Ok { message_id; message_type; message_timestamp }))
;;

let my_deserializer =
  let open Serde_json in
  Serde.De.(
    deserializer
    @@ fun ctx ->
    record ctx "" 1
    @@ fun ctx ->
    let* metadata = field ctx "metadata" (d deserialize_twitch_type_metadata) in
    match metadata.message_type with
    | "session_welcome" ->
      let* session =
        field ctx "payload" (d (with_payload deserialize_twitch_type_session))
      in
      Ok (SessionWelcome { session })
    | _ -> assert false)
;;

let get_twitch_message msg = Serde_json.of_string my_deserializer msg
