open Riot

open Logger.Make (struct
    let namespace = [ "subd"; "requests" ]
  end)

let ( let* ) = Result.bind

type request_result =
  { status : Http.Status.t [@printer Http.Status.pp]
  ; headers : Http.Header.t [@printer Http.Header.pp_hum]
  ; body : string
  }
[@@deriving show]

(* let* conn, [ `Status status; `Headers headers ] = Blink.stream conn in *)
(* let* conn, [ `Data body ] = Blink.stream conn in *)
(* let* _conn, [ `Done ] = Blink.stream conn in *)
let stream_until_done conn =
  let status_ref = ref None in
  let headers_ref = ref None in
  let body_ref = ref None in
  let rec aux conn recv =
    match recv with
    | [] ->
      debug (fun f -> f "more streaming");
      let* conn, resp = Blink.stream conn in
      aux conn resp
    | `Status status :: rest ->
      info (fun f -> f "Status: %a@." Http.Status.pp status);
      status_ref := Some status;
      aux conn rest
    | `Headers headers :: rest ->
      trace (fun f -> f "Headers: %a" Http.Header.pp_hum headers);
      headers_ref := Some headers;
      aux conn rest
    | `Data data :: rest ->
      trace (fun f -> f "got data");
      body_ref := Some data;
      aux conn rest
    | `Done :: [] -> Ok ()
    | _ -> failwith "unexpected response: parsing"
  in
  let* () = aux conn [] in
  match !status_ref, !headers_ref, !body_ref with
  | Some status, Some headers, Some body ->
    let body = Riot.Bytestring.to_string body in
    Ok (conn, { status; headers; body })
  | _ -> failwith "unexpected response: not all available"
;;
