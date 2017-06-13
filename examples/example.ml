open Weeping
open Weeping.Operator

type foo = {a: int; b: string}

let init_foo a b = {a;b}

(* example 1 *)

let _ =
 let json = Js.Json.parseExn "{\"x\":6}" in
 match ( json <| prop "x" Int) with
 | Some n -> print_int n
 | None -> ()

(* exapmle 2 *)
let _ =
 let json = Js.Json.parseExn "{\"x\":{\"y\":\"Hey\"}}" in
 match (json <| path ["x"; "y"] String) with
 | Some str -> print_endline str
 | None -> ()
