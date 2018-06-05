module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val find_opt : key -> 'a t -> 'a option

  val min_binding_opt : 'a t -> (key * 'a) option

  val max_binding_opt : 'a t -> (key * 'a) option

  val choose_opt : 'a t -> (key * 'a) option

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val of_seq : (key * 'a) Stdcompat__seq.t -> 'a t

  val add_seq : (key * 'a) Stdcompat__seq.t -> 'a t -> 'a t

  val to_seq : 'a t -> (key * 'a) Stdcompat__seq.t

  val to_seq_from : key -> 'a t -> (key * 'a) Stdcompat__seq.t
end

module Make (Ord : OrderedType) : sig
  include module type of struct
    include Map.Make (Ord)
  end

  include S with type 'a t := 'a t and type key := key
end
