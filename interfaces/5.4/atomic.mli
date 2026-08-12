type !'a t
val make : 'a -> 'a t
val make_contended : 'a -> 'a t
val get : 'a t -> 'a
val set : 'a t -> 'a -> unit
val exchange : 'a t -> 'a -> 'a
val compare_and_set : 'a t -> 'a -> 'a -> bool
val fetch_and_add : int t -> int -> int
val incr : int t -> unit
val decr : int t -> unit
module Loc :
sig
  type 'a t = 'a atomic_loc
  external get : 'a t -> 'a = "%atomic_load_loc"
  val set : 'a t -> 'a -> unit
  external exchange : 'a t -> 'a -> 'a = "%atomic_exchange_loc"
  external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas_loc"
  external fetch_and_add : int t -> int -> int = "%atomic_fetch_add_loc"
  val incr : int t -> unit
  val decr : int t -> unit
end
