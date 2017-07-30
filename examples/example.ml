open Weeping
open Weeping.Operator
open Weeping.OptionOperator

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

(* example 4 *)
let match_users = Match(select_list match_user)

let _ =
  match (json <| prop "friends" match_users) with
    | Some users -> users |> List.iter show_user
    | None -> ()
