(* json type *)
type _ kind =
  | Void : unit kind
  | Bool : bool kind
  | Int : int kind
  | Float : float kind
  | String : string kind
  | Object : string * 'a kind -> 'a kind
  | Match: (Js.Json.t -> 'b option) -> 'b kind

let select (type a) (query: a kind) json : a option =
  let rec prop json (query: a kind): a option = match query with
    | Bool -> begin match Js.Json.reifyType json with
        | (Js.Json.Boolean, b) -> Some (Js.to_bool b)
        | _ -> None
      end
    | Int -> begin match Js.Json.reifyType json with
        | (Js.Json.Number, n) -> Some (int_of_float n)
        | _ -> None
      end
    | Float -> begin match Js.Json.reifyType json with
        | (Js.Json.Number, n) -> Some n
        | _ -> None
      end
    | String -> begin match Js.Json.reifyType json with
        | (Js.Json.String, str) -> Some str
        | _ -> None
      end
    | Match f -> f json
    | Object(key, q) -> begin match Js.Json.reifyType json with
        | (Js.Json.Object, obj) -> begin match Js.Dict.get obj key with
            | Some x -> prop x q
            | _ -> None
          end
        | _ -> None
      end
    | _ -> None
  in prop json query

let select_option_list (type a) (query: a kind) json: a option list option = match Js.Json.reifyType json with
  | (Js.Json.Array, arr) -> Some(Array.to_list arr |> List.rev_map (select query) |> List.rev)
  | _ -> None

let select_list (type a) (query: a kind) json: a list option =
  match Js.Json.reifyType json with
  | (Js.Json.Array, arr) -> Some(Array.to_list arr |>
                                 List.rev_map (select query) |>
                                 List.fold_left (fun acc -> function Some x -> x :: acc | None -> acc) [])
  | _ -> None

let select_tuple2 (type a) (type b) ((qa, qb): a kind * b kind) json: (a * b) option = match Js.Json.reifyType json with
  | (Js.Json.Array, arr) ->
    if Array.length arr > 1 then begin match (select qa (Array.get arr 0), select qb (Array.get arr 1)) with
      | (Some a, Some b) -> Some(a, b)
      | _ -> None
    end
    else
      None
  | _ -> None

let prop key kind = Object(key, kind)

let rec path keys kind = match keys with
  | x :: xs -> Object(x, path xs kind)
  | [] -> kind

module Operator = struct
  let ( <| ) (type a) json (query: a kind) = select query json
  let ( <|| ) (type a) json (query: a kind): a list option = select_list query json
  let ( <||? ) (type a) json (query: a kind): a option list option = select_option_list query json
  let ( <|* ) (type a) (type b) json ((qa, qb): a kind * b kind): (a * b) option = select_tuple2 (qa, qb) json
end


module Option = struct
  let ( >>= ) a f = match a with
    | Some x -> f x
    | _ -> None

  let ( <$> ) a f = match a with
    | Some x -> Some(f x)
    | _ -> None

  let ( <*> ) af a = match (af, a) with
    | (Some f, Some x) -> Some(f x)
    | _ -> None
end
