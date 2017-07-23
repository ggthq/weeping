type _ kind =
  | Null : unit kind
  | Bool : bool kind
  | Int : int kind
  | Float : float kind
  | String : string kind
  | Object : string * 'a kind -> 'a kind
  | Match : (Js.Json.t -> 'b option) -> 'b kind

module StringMap :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end

val select : 'a kind -> Js.Json.t -> 'a option
val select_option_list : 'a kind -> Js.Json.t -> 'a option list option
val select_list : 'a kind -> Js.Json.t -> 'a list option
val select_map : 'a kind -> Js.Json.t -> 'a StringMap.t option
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
