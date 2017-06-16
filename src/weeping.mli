type _ kind =
  | Null : unit kind
  | Bool : bool kind
  | Int : int kind
  | Float : float kind
  | String : string kind
  | Object : string * 'a kind -> 'a kind
  | Match : (Js.Json.t -> 'b option) -> 'b kind
val select : 'a kind -> Js.Json.t -> 'a option
val select_option_list : 'a kind -> Js.Json.t -> 'a option list option
val select_list : 'a kind -> Js.Json.t -> 'a list option
val select_tuple2 : 'a kind * 'b kind -> Js.Json.t -> ('a * 'b) option
val prop : string -> 'a kind -> 'a kind
val path : string list -> 'a kind -> 'a kind
module Operator :
  sig
    val ( <| ) : Js.Json.t -> 'a kind -> 'a option
    val ( <|| ) : Js.Json.t -> 'a kind -> 'a list option
    val ( <||? ) : Js.Json.t -> 'a kind -> 'a option list option
    val ( <|* ) : Js.Json.t -> 'a kind * 'b kind -> ('a * 'b) option
  end
module OptionOperator :
  sig
    val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
    val ( <$> ) : 'a option -> ('a -> 'b) -> 'b option
    val ( <*> ) : ('a -> 'b) option -> 'a option -> 'b option
    val ( <!> ) : 'a option -> 'a option -> 'a option
  end
