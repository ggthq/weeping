# weeping

Functional JSON parsing library for BuckleScript inspired by [Argo](https://github.com/thoughtbot/Argo)

## How to use

### 1 getting number

JSON

```json
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

```json
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

### 3 record and Match

JSON

```json
{
  "x": {
    "key1": "Hello",
    "key2": 5
  }
}
```

```OCaml
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
```
