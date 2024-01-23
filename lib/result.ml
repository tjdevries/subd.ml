include Stdlib.Result

let ( let+ ) result f = map f result
let ( let* ) = bind

let ( and* ) r1 r2 =
  match r1, r2 with
  | Ok x, Ok y -> Ok (x, y)
  | Ok _, Error e | Error e, Ok _ | Error e, Error _ -> Error e
;;
