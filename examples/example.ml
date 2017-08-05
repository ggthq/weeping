open Weeping
open Weeping.Operator
open Weeping.OptionOperator
open Weeping.ParserOperator

(* example 1 *)

let file = Node.Fs.readFileSync "./examples/test.json" `utf8
let json = Js.Json.parseExn file

let _ =
  match ( json <| prop "x" Int) with
    | Some n -> print_int n; print_newline()
    | None -> ()

(* exapmle 2 *)
let _ =
  match (json <| path ["this"; "is"] String) with
    | Some str -> print_endline str
    | None -> ()

(* example 3 *)

type user = {
  name: string;
  age: int;
}

let init_user name age = {name;age;}

let match_user = Match(fun json ->
  Some init_user <*>
  (json <| prop "name" String) <*>
  (json <| prop "age" Int))

let show_user {name; age;} = print_string ("My name is " ^ name ^ ", ");
  print_int age;
  print_string " years old";
  print_newline()

let _ =
  match (json <| prop "me" match_user) with
    | Some user -> show_user user
    | None -> ()

(* example 4 list *)
let match_users = Match(select_list match_user)

let _ =
  match (json <| prop "friends" match_users) with
    | Some users -> users |> List.iter show_user
    | None -> ()

(* example 5  recursive *)

type 'a binary_tree =
  | Leaf of 'a
  | Tree of 'a binary_tree * 'a binary_tree

let init_leaf n = Leaf n
let init_tree left right = Tree(left, right)

let rec show_tree tree show = match tree with
  | Leaf n -> show n; print_newline()
  | Tree(l, r) ->
    show_tree l (fun a -> print_string "  "; show a; print_newline());
    show_tree r (fun a -> print_string "  "; show a; print_newline())

let rec match_tree =
  let leaf json = init_leaf <$> (json <| Int) in
  let tree json = Some init_tree
    <*> (json <| prop "right" match_tree)
    <*> (json <| prop "left" match_tree) in
  Match(fun json -> (leaf json) <!> (tree json))

let _ =
  match (json <| prop "root" match_tree) with
    | Some tree -> show_tree tree print_int
    | None -> print_string "Not match"

(* example 6 parser operator *)

(* let rec match_tree23 =
  let leaf = init_leaf <$$> select Int in
  Match(leaf <!!> (init_tree
    <$$> select (prop "right" match_tree23)
    <**> select (prop "left" match_tree23))) *)

let rec decode_tree json =
  let leaf = init_leaf <$$> select Int in
  let tree = init_tree
    <$$> select (prop "right" (Match decode_tree))
    <**> select (prop "left" (Match decode_tree)) in
  (leaf <!!> tree) json

let _ =
  match (json <| prop "root" (Match decode_tree)) with
    | Some tree -> show_tree tree print_int
    | None -> print_string "Not match"
