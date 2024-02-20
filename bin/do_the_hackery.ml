open Base

let () = ()

open Soup

let print_endline = Stdlib.print_endline

type field =
  { name : string
  ; type_ : twitch_type
  ; description : string
  }

and twitch_type =
  | String
  | Int
  | Bool
  | Object of field list
  | Array of twitch_type

let rec field_to_string field =
  Fmt.str
    "%s: %s | %s..."
    field.name
    (twitch_type_to_string field.type_)
    (String.sub
       field.description
       ~pos:0
       ~len:(min 15 (String.length field.description)))

and twitch_type_to_string = function
  | String -> "String"
  | Int -> "Int"
  | Bool -> "Bool"
  | Object _ -> "<object>"
  | Array type_ -> Fmt.str "Array<%s>" (twitch_type_to_string type_)
;;

let rec twitch_type_of_string = function
  | "string" -> String
  | "int" -> Int
  | "bool" -> Bool
  | "object" -> Object []
  | str when String.is_suffix str ~suffix:"[]" ->
    let type_ = String.sub str ~pos:0 ~len:(String.length str - 2) in
    Array (twitch_type_of_string type_)
  | _ -> failwith "Unknown type"
;;

let () =
  let soup = read_file "./data/websocket-ref.html" |> parse in
  let header = soup $ "#channel-chat-clear-event" in
  let request = header |> R.leaf_text in
  let event_id = id header |> Option.value_exn in
  Fmt.pr "Parsing: %s - %s@." request event_id;
  let table = header |> next_element |> Option.value_exn in
  let body = select "tbody" table |> first |> Option.value_exn in
  let rows = select "tr" body in
  let fields =
    fold
      (fun acc row ->
        let data = select "td" row in
        let name =
          nth 1 data |> Option.value_exn |> texts |> String.concat ~sep:"|"
        in
        let get_text idx =
          nth idx data |> Option.value_exn |> texts |> String.concat ~sep:" "
        in
        let type_ = get_text 2 |> twitch_type_of_string in
        let description = get_text 3 in
        { name; type_; description } :: acc)
      []
      rows
    |> List.rev
  in
  List.iter fields ~f:(fun field -> print_endline (field_to_string field));
  print_endline (name body);
  ()
;;
