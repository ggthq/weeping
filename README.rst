======
weeping
======

.. image:: https://img.shields.io/badge/License-MIT-yellow.svg
   :target: https://opensource.org/licenses/MIT
   :alt: License: MIT


The Functional JSON parsing library for BuckleScript inspired by `Argo <https://github.com/thoughtbot/Argo>`_

How to use
======

1 Getting number
------

* JSON

.. code-block:: JSON

  {
    "x": 6
  }


Example 1
^^^^^^^^

* OCaml

.. code-block:: OCaml

 let _ =
  let json = Js.Json.parseExn "{\"x\":6}" in
  match (json <| prop "x" Int) with
   | Some n -> print_int n
   | None -> ()

..note::

  `( <| )` is function type is`Js.Json.t -> 'a kind -> 'a option`

  and

  `prop "x" Int` is `int kind`

  `prop "x" Int` is syntax sugar `Object("x", Int)`

2 Nesting value
------

* JSON

.. code-block:: JSON

  {
    "x": {
      "y": "Hey"
    }
  }


Example 2
^^^^^^^^

* OCaml

.. code-block:: OCaml

 let _ =
  let json = Js.Json.parseExn "{\"x\":{\"y\":\"Hey\"}}" in
  match (json <| path ["x"; "y"] String) with
  | Some str -> print_endline str
  | None -> ()


.. note::

  `path ["x"; "y"] String` is `string kind`

  `path ["x"; "y"] String` is syntax sugar `Object("x", Object("y", String))`

3 Record Type and Pattern Matching
^^^^^^^^

* JSON

.. code-block:: json

  {
    "x": {
      "key1": "Hello",
      "key2": 5
    }
  }


.. code-block:: OCaml

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

Authors
======
@kdxu, @hiroqn

License
======
This project is licensed under the MIT License - see the `LICENSE.md <./LICENSE.md>`_ file for details




