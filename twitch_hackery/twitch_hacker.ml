open Base
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
  | Object of
      { name : string
      ; fields : field list
      }
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
  | "integer" -> Int
  | "bool" -> Bool
  | "boolean" -> Bool
  | "object" -> Object { name = "object"; fields = [] }
  | str when String.is_suffix str ~suffix:"[]" ->
    let type_ = String.sub str ~pos:0 ~len:(String.length str - 2) in
    Array (twitch_type_of_string type_)
  | str -> Fmt.failwith "Unknown type: '%s'" str
;;

let parse_table_body body =
  let rows = select "tr" body in
  let nodes = to_list rows in
  let indent = ref 0 in
  let fields =
    List.fold nodes ~init:[] ~f:(fun acc row ->
      let data = select "td" row in
      let name =
        nth 1 data |> Option.value_exn |> texts |> String.concat ~sep:"|"
      in
      let type_ =
        let type_text = nth 2 data |> Option.value_exn |> texts in
        let text_count = List.length type_text in
        match text_count, type_text with
        | 1, [ type_ ] when !indent = 0 ->
          twitch_type_of_string (String.lowercase type_)
        | 1, [ _spacing; _type_ ] when !indent = 0 ->
          Object { name = "spaced:object"; fields = [] }
        | _, [ type_ ] -> twitch_type_of_string (String.lowercase type_)
        | _ -> assert false
      in
      (* Description is in third *)
      let description =
        nth 3 data |> Option.value_exn |> texts |> String.concat ~sep:" "
      in
      { name; type_; description } :: acc)
    |> List.rev
  in
  (* let fields = *)
  (*   fold *)
  (*     (fun acc row -> *)
  (*       let data = select "td" row in *)
  (*       let name = *)
  (*         nth 1 data |> Option.value_exn |> texts |> String.concat ~sep:"|" *)
  (*       in *)
  (*       let get_text idx = *)
  (*         nth idx data |> Option.value_exn |> texts |> String.concat ~sep:" " *)
  (*       in *)
  (*       let type_ = get_text 2 |> String.lowercase |> twitch_type_of_string in *)
  (*       let description = get_text 3 in *)
  (*       { name; type_; description } :: acc) *)
  (*     [] *)
  (*     rows *)
  (*   |> List.rev *)
  (* in *)
  fields
;;

let run () =
  let soup = read_file "./data/websocket-ref.html" |> parse in
  let header = soup $ "#channel-chat-clear-event" in
  let request = header |> R.leaf_text in
  let event_id = id header |> Option.value_exn in
  Fmt.pr "Parsing: %s - %s@." request event_id;
  let table = header |> next_element |> Option.value_exn in
  let body = select "tbody" table |> first |> Option.value_exn in
  let fields = parse_table_body body in
  List.iter fields ~f:(fun field -> print_endline (field_to_string field));
  print_endline (name body);
  ()
;;

let%expect_test "addition" =
  Fmt.pr "%d" (1 + 4);
  [%expect {| 5 |}]
;;

let rec write_field_type = function
  | String -> "string", []
  | Int -> "int", []
  | Bool -> "bool", []
  | Object obj -> obj.name, obj.fields
  | Array type_ ->
    let name, remaining = write_field_type type_ in
    Fmt.str "%s list" name, remaining
;;

type x = { thing : x_thing }
and x_thing = { y : string }

let write_fields name fields =
  let aux name fields remaining =
    let remaining = ref remaining in
    Fmt.pr "type %s = {@." name;
    List.iter fields ~f:(fun field ->
      let type_str, additional = write_field_type field.type_ in
      Fmt.pr "  %s: %s (** %s *);\n" field.name type_str field.description;
      remaining := additional :: !remaining);
    Fmt.pr "}@."
  in
  aux name fields []
;;

let%expect_test "channel-chat-clear-event" =
  let soup = parse Test_divs.channel_clear_event in
  let header = soup $ "#channel-chat-clear-event" in
  let header_name =
    id header
    |> Option.value_exn
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
  in
  let table = header |> next_element |> Option.value_exn in
  let body = select "tbody" table |> first |> Option.value_exn in
  let fields = parse_table_body body in
  write_fields header_name fields;
  [%expect
    {|
    type channel_chat_clear_event = {
      broadcaster_user_id: string (** The broadcaster user ID. *);
      broadcaster_user_name: string (** The broadcaster display name. *);
      broadcaster_user_login: string (** The broadcaster login. *);
    } |}]
;;

let%expect_test "charity-donation-event" =
  let soup = parse Test_divs.charity_donation_event in
  let header = soup $ "#charity-donation-event" in
  let header_name =
    id header
    |> Option.value_exn
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
  in
  (* TODO: We have to handle finding the next table *)
  let table =
    header
    |> next_element
    |> Option.value_exn
    |> next_element
    |> Option.value_exn
  in
  let body = select "tbody" table |> first |> Option.value_exn in
  let fields = parse_table_body body in
  write_fields header_name fields;
  [%expect
    {|
    type charity_donation_event = {
      id: string (** An ID that identifies the donation. The ID is unique across campaigns. *);
      charity_logo: string (** A URL to an image of the charity’s logo. The image’s type is PNG and its size is 100px X 100px. *);
      amount: object (** An object that contains the amount of money that the user donated. *);
         |value: int (** The monetary amount. The amount is specified in the currency’s minor unit. For example, the minor units for USD is cents, so if the amount is $5.50 USD,  value  is set to 550. *);
         |decimal_places: int (** The number of decimal places used by the currency. For example, USD uses two decimal places. Use this number to translate  value  from minor units to major units by using the formula: value / 10^decimal_places *);
         |currency: string (** The ISO-4217 three-letter currency code that identifies the type of currency in  value . *);
    } |}]
;;
