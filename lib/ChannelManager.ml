open Riot

let chan_mgr = "subd.manager"

open Logger.Make (struct
    let namespace = [ "subd"; "manager" ]
  end)

type channel =
  { name : string [@warning "-69"]
  ; clients : Pid.t list
  }

module Store = Store.Make (struct
    type key = string
    type value = channel
  end)

type state = { channels : (string, channel) Hashtbl.t }

type Message.t +=
  | Register of
      { channel : string
      ; client : Pid.t
      }
  | Broadcast of
      { channel : string
      ; msg : Message.t
      }

let rec loop state =
  match receive () with
  | Register { channel; client } -> handle_register state channel client
  | Broadcast { channel; msg } -> handle_broadcast state channel msg
  | _ -> loop state

and handle_register state channel_name client =
  let channel =
    match Hashtbl.find_opt state.channels channel_name with
    | None -> { name = channel_name; clients = [ client ] }
    | Some chan -> { chan with clients = client :: chan.clients }
  in
  Hashtbl.replace state.channels channel_name channel;
  loop state

and handle_broadcast state channel msg =
  let clients =
    Hashtbl.find_opt state.channels channel
    |> Option.map (fun chan -> chan.clients)
    |> Option.value ~default:[]
  in
  clients |> List.iter (fun client -> send client msg);
  loop state
;;

let init () =
  let state = { channels = Hashtbl.create 0 } in
  loop state
;;

let start_link () =
  let pid = spawn_link init in
  register chan_mgr pid;
  Ok pid
;;

let register client ~channel =
  let mgr = Process.where_is chan_mgr |> Option.get in
  send mgr (Register { channel; client })
;;

let broadcast msg ~channel =
  let mgr = Process.where_is chan_mgr |> Option.get in
  send mgr (Broadcast { channel; msg })
;;
