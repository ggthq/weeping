# weeping

Functional JSON parsing library for BuckleScript inspired by [Argo](https://github.com/thoughtbot/Argo)

## How to use

### 1 getting number

JSON

```
{
  "x": 6
}
```

example 1

```OCaml
let _ =
 let json = Js.Json.parseExn "{\"x\":6}" in
 match (json <| prop "x" Int) with
 | Some n -> print_int n
 | None -> ()
```

`( <| )` is function type is`Js.Json.t -> 'a kind -> 'a option`

and

`prop "x" Int` is `int kind`

`prop "x" Int` is syntax sugar `Object("x", Int)`

### 2 nesting value

JSON

```
{
  "x": {
    "y": "Hey"
  }
}
```

example 2

```OCaml
let _ =
 let json = Js.Json.parseExn "{\"x\":{\"y\":\"Hey\"}}" in
 match (json <| path ["x"; "y"] String) with
 | Some str -> print_endline str
 | None -> ()
```

`path ["x"; "y"] String` is `string kind`

`path ["x"; "y"] String` is syntax sugar `Object("x", Object("y", String))`


(*

let t1 j = j <|| Bool

let t2 j = j <| prop "x" (Func(fun j -> j <|* (String, Int)))

let t3 j = j <| path ["x"; "y"] (Func(fun j -> j <|| String))

let t4 j = j <| path ["ss"; "dd"; "ff"] Int

let t5 j = Some init_foo <*> (j <| Int) <*> (j <| String)

let () = let j = Js.Json.parseExn "{\"x\": 3}" in
       ignore (Some print_int <*> (j <| prop "x" Int)) *)
