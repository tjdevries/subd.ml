module Messages = struct
  (*   "metadata": { *)
  (*     "message_id": "96a3f3b5-5dec-4eed-908e-e11ee657416c", *)
  (*     "message_type": "session_welcome", *)
  (*     "message_timestamp": "2023-07-19T14:56:51.634234626Z" *)
  (*   }, *)
  type metadata =
    { message_id : string
    ; message_type : string
    ; message_timestamp : string
    ; subscription_type : string option
    ; subscription_version : string option
    }
  [@@deriving serialize, deserialize]

  (* "session": { *)
  (*   "id": "AQoQILE98gtqShGmLD7AM6yJThAB", *)
  (*   "status": "connected", *)
  (*   "connected_at": "2023-07-19T14:56:51.616329898Z", *)
  (*   "keepalive_timeout_seconds": 10, *)
  (*   "reconnect_url": null *)
  (* } *)
  type session =
    { id : string
    ; status : string
    ; connected_at : string
    ; keepalive_timeout_seconds : int
    ; reconnect_url : string option
    }
  [@@deriving serialize, deserialize]

  type subscription_condition = { broadcaster_user_id : string }
  [@@deriving serialize, deserialize]

  type transport =
    { method_ : string [@serde { rename = "method" }]
    ; session_id : string
    }
  [@@deriving serialize, deserialize]

  type subscription =
    { id : string
    ; type_ : string [@serde { rename = "type" }]
    ; version : string
    ; status : string
    ; cost : int
    ; condition : subscription_condition
    ; transport : transport
    ; created_at : string
    }
  [@@deriving serialize, deserialize]

  type event =
    { broadcaster_user_id : string
    ; broadcaster_user_login : string
    ; broadcaster_user_name : string
    ; title : string
    ; language : string
    ; category_id : string
    ; category_name : string
    ; is_mature : bool
    }
  [@@deriving serialize, deserialize]

  type subscription_gift_event =
    { user_id : string
    ; user_login : string
    ; user_name : string
    ; broadcaster_user_id : string
    ; broadcaster_user_login : string
    ; broadcaster_user_name : string
    ; total : int
    ; tier : string
    ; cumulative_total : int
    ; is_anonymous : bool
    }
  [@@deriving serialize, deserialize]
end

include Messages

type base_twitch_message = { metadata : metadata }
[@@deriving serialize, deserialize]

type twitch_message =
  | SessionWelcome of { session : session }
  | KeepAlive
  | ChannelUpdate of
      { subscription : subscription
      ; event : event
      }
  | ChannelSubscriptionGift of { event : subscription_gift_event }

let deserialize_notification_payload metadata =
  let ( let* ) = Serde_json.( let* ) in
  Serde.De.(
    deserializer
    @@ fun ctx ->
    record ctx "payload" 1 (fun ctx ->
      match metadata.subscription_type with
      | Some "channel.update" ->
        let* subscription =
          field ctx "subscription" (d deserialize_subscription)
        in
        let* event = field ctx "event" (d deserialize_event) in
        Ok (ChannelUpdate { subscription; event })
      (* | Some "channel.chat.message" -> *)
      (*   Ok (ChannelChatMessage { message = "hello" }) *)
      | _ ->
        Fmt.failwith
          "Got a notification for: %s"
          (metadata.subscription_type |> Option.value ~default:"")))
;;

module PayloadSession = struct
  type payload_session = { session : session }
  [@@deriving serialize, deserialize]

  type t =
    { metadata : metadata
    ; payload : payload_session
    }
  [@@deriving serialize, deserialize]
end

module PayloadChannelUpdate = struct
  type payload =
    { subscription : subscription
    ; event : event
    }
  [@@deriving serialize, deserialize]

  type t =
    { metadata : metadata
    ; payload : payload
    }
  [@@deriving serialize, deserialize]

  let deserialize_t ctx =
    deserialize_t ctx
    |> Result.map (fun { payload; _ } ->
      ChannelUpdate
        { subscription = payload.subscription; event = payload.event })
  ;;
end

module PayloadChannelSubscriptionGift = struct
  type payload =
    { subscription : subscription
    ; event : subscription_gift_event
    }

  and t =
    { metadata : metadata
    ; payload : payload
    }
  [@@deriving serialize, deserialize]

  let deserialize_t ctx =
    deserialize_t ctx
    |> Result.map (fun { payload; _ } ->
      ChannelSubscriptionGift { event = payload.event })
  ;;
end

let make_payload_visitor name ctx ~visit_string ~visit_tag =
  let open Serde in
  let open Serde.De in
  record ctx name 1 (fun ctx ->
    let field_visitor =
      Visitor.make ~visit_string ~visit_int:(fun _ctx _ -> Ok `invalid_tag) ()
    in
    let* tag = next_field ctx field_visitor in
    visit_tag tag)
;;

let retrieve_metadata =
  let open Serde in
  Serde.De.(
    deserializer
    @@ fun ctx ->
    record ctx "" 1 (fun ctx ->
      let field_visitor =
        let visit_string _ctx str =
          match str with
          | "metadata" -> Ok `metadata
          | _ -> Error `invalid_tag
        in
        let visit_int _ctx str =
          match str with
          | 0 -> Ok `metadata
          | _ -> Error `invalid_tag
        in
        Visitor.make ~visit_string ~visit_int ()
      in
      let* tag = next_field ctx field_visitor in
      match tag with
      | Some `metadata -> field ctx "metadata" (d deserialize_metadata)
      | None -> Error `missing_field))
;;

let get_twitch_message msg =
  let* metadata = Serde_json.of_string retrieve_metadata msg in
  match metadata.message_type, metadata.subscription_type with
  | "session_welcome", _ ->
    let* payload = Serde_json.of_string PayloadSession.deserialize_t msg in
    Ok (SessionWelcome { session = payload.payload.session })
  | "notification", Some "channel.update" ->
    Serde_json.of_string PayloadChannelUpdate.deserialize_t msg
  | "notification", Some "channel.subscription.gift" ->
    Serde_json.of_string PayloadChannelSubscriptionGift.deserialize_t msg
  | "notification", Some notification ->
    Error (`Msg (Fmt.str "Unhandled notification: %s" notification))
  | message_type, _ ->
    Error (`Msg (Fmt.str "Unhandled message type: %s" message_type))
;;
