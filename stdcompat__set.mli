module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  val find : elt -> t -> elt

  val of_list : elt list -> t

  val map : (elt -> elt) -> t -> t

  val min_elt_opt : t -> elt option

  val max_elt_opt : t -> elt option

  val choose_opt : t -> elt option

  val find_opt : elt -> t -> elt option

  val find_first : (elt -> bool) -> t -> elt
  (** @before 4.05.0 linear time complexity. *)

  val find_first_opt : (elt -> bool) -> t -> elt option
  (** @before 4.05.0 linear time complexity. *)

  val find_last : (elt -> bool) -> t -> elt
  (** @before 4.05.0 linear time complexity. *)

  val find_last_opt : (elt -> bool) -> t -> elt option
  (** @before 4.05.0 linear time complexity. *)

  val of_seq : elt Stdcompat__seq.t -> t

  val add_seq : elt Stdcompat__seq.t -> t -> t

  val to_seq : t -> elt Stdcompat__seq.t

  val to_seq_from : elt -> t -> elt Stdcompat__seq.t
end

module Make (Ord : OrderedType) : sig
  include module type of struct
    include Set.Make (Ord)
  end

  include S with type elt := elt and type t := t
end
