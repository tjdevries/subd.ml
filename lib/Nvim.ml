open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type nvim_message = { event : string }
[@@deriving yojson] [@@yojson.allow_extra_fields]
