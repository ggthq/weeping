open Weeping
open Weeping.Operator
open Weeping.OptionOperator

(* example 1 *)

let _ =
 let json = Js.Json.parseExn "{\"x\":6}" in
 match ( json <| prop "x" Int) with
 | Some n -> print_int n; print_newline()
 | None -> ()

(* exapmle 2 *)
let _ =
 let json = Js.Json.parseExn "{\"x\":{\"y\":\"Hey\"}}" in
 match (json <| path ["x"; "y"] String) with
 | Some str -> print_endline str
 | None -> ()

(* example 3 *)

type foo = {
  str: string;
  num: int;
}

let init_foo str num = {str;num;}

let match_foo json = Some init_foo <*> (json <| prop "key1" String) <*> (json <| prop "key2" Int)

let _ =
 let json = Js.Json.parseExn "{\"x\":{\"key1\":\"Hello\",\"key2\":5}}" in
 match (json <| prop "x" (Match match_foo)) with
 | Some {str; num} -> print_string str; print_int num; print_newline()
 | None -> ()
