open Riot
open Trail
open Router

module Memory = struct
  (** A small trail to serve static assets from in-memory modules ideal
      for use with ocaml-crunch. *)

  open Riot.Logger.Make (struct
      let namespace = [ "trail"; "memory" ]
    end)

  module type AssetLibrary = sig
    val read : string -> string option
  end

  type args =
    { prefix : string
    ; modules : (module AssetLibrary) list
    }

  type state = args

  let rec strip_leading_slash path =
    if String.starts_with ~prefix:"/" path
    then strip_leading_slash (String.sub path 1 (String.length path - 1))
    else path
  ;;

  let config ?(prefix = "/static") modules =
    let prefix = strip_leading_slash prefix in
    { prefix; modules }
  ;;

  let init args = args

  let rec find_file path modules =
    match modules with
    | [] -> None
    | (module Assets : AssetLibrary) :: modules ->
      (match Assets.read path with
       | Some file -> Some (Bytestring.of_string file)
       | None -> find_file path modules)
  ;;

  let call (conn : Conn.t) t =
    let rel_path = conn.req.path |> String.concat Stdlib.Filename.dir_sep in
    debug (fun f -> f "serving file at %S" rel_path);
    if String.starts_with ~prefix:t.prefix rel_path
    then (
      let path = Stringext.replace_all ~pattern:t.prefix ~with_:"" rel_path in
      match find_file path t.modules with
      | Some file ->
        let mime_type = Magic_mime.lookup path in
        conn
        |> Conn.with_header "content-type" mime_type
        |> Conn.send_response `OK file
      | None -> conn |> Conn.send_response `Not_found {%b||})
    else conn
  ;;
end

let trail =
  [ use (module CORS) CORS.(config ~origin:"*" ())
  ; use (module Logger) Logger.(args ~level:Debug ())
  ; use
      (module Memory)
      Memory.(config ~prefix:"/static" [ (module Frontend_assets) ])
  ; router []
  ]
;;

module App = struct
  let start () =
    let handler = Nomad.trail trail in
    Nomad.start_link ~port:8080 ~handler ()
  ;;
end

let () = Riot.start ~apps:[ (module App) ] ()
