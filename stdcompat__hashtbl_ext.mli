module type SeededHashedType  =
  sig type t val equal : t -> t -> bool val hash : int -> t -> int end

(*
module UnseedHashedType (H : SeededHashedType) : Hashtbl.HashedType
with type t = H.t
*)

module MakeSeeded (H : SeededHashedType) : sig

  type 'a t = 'a Hashtbl.MakeSeeded(H).t

(*
  type 'a t = 'a Hashtbl.Make(UnseedHashedType(H)).t
*)
end

type statistics

  = Hashtbl.statistics

  = {
    num_bindings : int;
    num_buckets : int;
    max_bucket_length : int;
    bucket_histogram : int array;
  }


type ('a, 'b) bucketlist =
  | Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist


type ('a, 'b) internal =
    { mutable size: int;
      mutable data: ('a, 'b) bucketlist array;
      mutable seed: int;
      mutable initial_size: int;
    }

(*

type ('a, 'b) internal =
    { mutable size: int;
      mutable data: ('a, 'b) bucketlist array;
      mutable seed: int;
      initial_size: int;
    }

(*
type ('a, 'b) internal =
    { mutable size: int;
      mutable data: ('a, 'b) bucketlist array;
    }
*)
*)

val filter_map_inplace : ('a -> 'b -> 'b option) -> 'c -> unit

val to_seq : 'a -> ('b * 'c) Stdcompat__seq.t

val to_seq_keys : 'a -> 'b Stdcompat__seq.t

val to_seq_values : 'a -> 'b Stdcompat__seq.t

val stats : 'a -> statistics

(*
type ('table, 'key, 'value) dict = {
    clear : 'table -> unit;
    fold : 'a . ('key -> 'value -> 'a -> 'a) -> 'table -> 'a -> 'a;
    add : 'table -> 'key -> 'value -> unit;
    remove : 'table -> 'key -> unit;
    replace : 'table -> 'key -> 'value -> unit;
  }

val filter_map_inplace : ('table, 'key, 'value) dict ->
    ('key -> 'value -> 'value option) -> 'table -> unit

val to_seq :
    (('key -> 'value -> ('key * 'value) list -> ('key * 'value) list) ->
      'table -> ('key * 'value) list -> ('key * 'value) list)
      -> 'table -> ('key * 'value) Stdcompat__seq.t

val to_seq_keys :
    (('key -> 'value -> ('key * 'value) list -> ('key * 'value) list) ->
      'table -> ('key * 'value) list -> ('key * 'value) list)
      -> 'table -> 'key Stdcompat__seq.t

val to_seq_values :
    (('key -> 'value -> ('key * 'value) list -> ('key * 'value) list) ->
      'table -> ('key * 'value) list -> ('key * 'value) list)
      -> 'table -> 'value Stdcompat__seq.t

val stats : length:('table -> int) -> 'table -> statistics
*)

val add_seq : ('table -> 'key -> 'value -> unit) -> 'table ->
     ('key * 'value) Stdcompat__seq.t -> unit

val of_seq : create:(int -> 'table) ->
  replace:('table -> 'key -> 'value -> unit) ->
    ('key * 'value) Stdcompat__seq.t -> 'table
