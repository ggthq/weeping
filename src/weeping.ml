type 'a decoder = Js.Json.t -> 'a option

(* json type *)
type _ kind =
  | Null : unit kind
  | Bool : bool kind
  | Int : int kind
  | Float : float kind
  | Str : string kind
  | Object : string * 'a kind -> 'a kind
  | Decode : 'b decoder -> 'b kind

let op_map f a = match a with
  | Some x -> Some(f x)
  | _ -> None

(* Kind Util *)

let prop_kind key kind = Object(key, kind)

let rec path_kind keys kind = match keys with
  | x :: xs -> Object(x, path_kind xs kind)
  | [] -> kind

(* Decoder Util *)
let select (type a) (query: a kind) json : a option =
  let rec prop (query: a kind) json: a option = match query with
    | Null -> Js.Json.decodeNull json |> op_map ignore
    | Bool -> Js.Json.decodeBoolean json
    | Int -> Js.Json.decodeNumber json |> op_map  int_of_float
    | Float -> Js.Json.decodeNumber json
    | Str -> Js.Json.decodeString json
    | Decode f -> f json
    | Object(key, q) -> begin match Js.Json.decodeObject json with
        | Some obj -> begin match Js.Dict.get obj key with
            | Some x -> prop q x
            | _ -> None
          end
        | _ -> None
      end
  in prop query json

let prop key kind = select (Object(key, kind))

let path keys kind = select (path_kind keys kind)

let listify kind = Decode(fun json ->
    match Js.Json.decodeArray json with
    | Some arr -> Some(Array.to_list arr |>
                       List.rev_map (select kind) |>
                       List.fold_left (fun acc o -> match o with Some x -> x :: acc | None -> acc) [])
    | _ -> None)

let list_kind decoder = Decode(fun json ->
    match Js.Json.decodeArray json with
    | Some arr -> Some(Array.to_list arr |>
                       List.rev_map decoder |>
                       List.fold_left (fun acc o -> match o with Some x -> x :: acc | None -> acc) [])
    | _ -> None)

let select_option_list (type a) (query: a kind) json: a option list option =
  match Js.Json.decodeArray json with
  | Some arr -> Some(Array.to_list arr |> List.rev_map (select query) |> List.rev)
  | _ -> None

let select_list (type a) (query: a kind) json: a list option =
  match Js.Json.decodeArray json with
  | Some arr -> Some(Array.to_list arr |>
                     List.rev_map (select query) |>
                     List.fold_left (fun acc o -> match o with Some x -> x :: acc | None -> acc) [])
  | _ -> None

module StringMap = Map.Make(String)

let select_map (type a) (query: a kind) json: a StringMap.t option =
  match Js.Json.decodeObject json with
  | Some obj ->
    let entries = Js.Dict.entries obj in
    let accumulator m (key, o) = match select query o with
      | Some x ->  StringMap.add key x m
      | _ -> m in
    Some(Js.Array.reduce accumulator StringMap.empty entries)
  | _ -> None

let select_tuple2 (qa, qb) json = match Js.Json.decodeArray json with
  | Some arr ->
    if Array.length arr > 1 then
      let o_a = select qa (Array.get arr 0) in
      let o_b = select qb (Array.get arr 1) in
      begin match (o_a, o_b) with
        | (Some a, Some b) -> Some(a, b)
        | _ -> None
      end
    else
      None
  | _ -> None

let select_tuple3 (qa, qb, qc) json = match Js.Json.decodeArray json with
  | Some arr ->
    if Array.length arr > 2 then
      let o_a = select qa (Array.get arr 0) in
      let o_b = select qb (Array.get arr 1) in
      let o_c = select qc (Array.get arr 2) in
      begin match (o_a, o_b, o_c) with
        | (Some a, Some b, Some c) -> Some(a, b, c)
        | _ -> None
      end
    else
      None
  | _ -> None

module SelectorOperator = struct
  let ( <| ) (type a) json (query: a kind) = select query json
  let ( <|| ) (type a) json (query: a kind): a list option = select_list query json
  let ( <||? ) (type a) json (query: a kind): a option list option = select_option_list query json
  let ( <|* ) (type a) (type b) json ((qa, qb): a kind * b kind): (a * b) option = select_tuple2 (qa, qb) json
end

module DecoderOperator = struct
  let ( <*> ) af a = fun json -> match af json with
    | Some f -> (a json) |> op_map  f
    | _ -> None
  let ( <$> ) f a = fun json -> a json |> op_map f
  let ( <|> ) a b = fun json -> match a json with
    | Some a -> Some a
    | _ -> b json
end

module OptionOperator = struct
  let ( >>= ) a f = match a with
    | Some x -> f x
    | _ -> None

  let ( <$> ) = op_map

  let ( <*> ) af a = match (af, a) with
    | (Some f, Some x) -> Some(f x)
    | _ -> None

  let ( <|> ) a b = match (a, b) with
    | (Some _, _) -> a
    | (None, Some _) -> b
    | _ -> None
end
