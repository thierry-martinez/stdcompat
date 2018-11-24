external id : 'a -> 'a = "%identity"
(** @since 4.08.0: external id : 'a -> 'a = "%identity" *)

val const : 'a -> 'b -> 'a
(** @since 4.08.0: val const : 'a -> 'b -> 'a *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** @since 4.08.0: val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c *)

val negate : ('a -> bool) -> 'a -> bool
(** @since 4.08.0: val negate : ('a -> bool) -> 'a -> bool *)

