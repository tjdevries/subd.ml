open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type op_only = { op : int } [@@deriving yojson] [@@yojson.allow_extra_fields]

module type OP_CODE_BASE = sig
  type data [@@deriving yojson]

  val op : int
end

module type OP_CODE = sig
  type data [@@deriving yojson]

  val op : int
end

module OpCode (Base : OP_CODE_BASE) = struct
  include Base

  let to_json (d : data) : Yojson.Safe.t =
    `Assoc [ "op", `Int op; "d", yojson_of_data d ]
  ;;
end

(* {
     "op": 0,
     "d": {
       "obsWebSocketVersion": "5.1.0",
       "rpcVersion": 1
     }
   } *)
module HelloData = struct
  type t =
    { obs_websocket_version : string [@key "obsWebSocketVersion"]
    ; rpc_version : int [@key "rpcVersion"]
    }
  [@@deriving yojson]
end

module Hello = OpCode (struct
    let op = 0

    type data = HelloData.t [@@deriving yojson]
  end)

(* {
     "rpcVersion": number,
     "authentication": string(optional),
     "eventSubscriptions": number(optional) = (EventSubscription::All)
   } *)

module IdentifyData = struct
  type t =
    { rpc_version : int [@key "rpcVersion"]
    ; authentication : string option [@yojson.option]
    ; event_subscriptions : int option
         [@key "eventSubscriptions"] [@yojson.option]
    }
  [@@deriving yojson]
end

module Identify = OpCode (struct
    let op = 1

    type data = IdentifyData.t [@@deriving yojson]
  end)

type requests =
  | SetCurrentProgramScene of { sceneName : string }
  | SetSceneItemEnabled of
      { sceneName : string
      ; sceneItemId : int
      ; sceneItemEnabled : bool
      }
  | GetSceneItemList of { sceneName : string }
[@@deriving yojson]

module RequestData = struct
  type t =
    { request_type : string
    ; request_id : string
    ; request_data : Yojson.Safe.t
    }

  let t_of_request request_data : t =
    match yojson_of_requests request_data with
    | `List [ `String request_type; request_data ] ->
      { request_id = "i-dont-care"; request_type; request_data }
    | _ -> assert false
  ;;

  let yojson_of_t t : Yojson.Safe.t =
    `Assoc
      [ "requestId", `String "testing-12345"
      ; "requestType", `String t.request_type
      ; "requestData", t.request_data
      ]
  ;;

  (* We hopefully never have to decode this, so i'm going to skip *)
  let t_of_yojson _ = assert false
end

module Request = OpCode (struct
    let op = 6

    type data = RequestData.t [@@deriving yojson]
  end)
