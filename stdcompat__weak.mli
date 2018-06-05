type 'a t = 'a Weak.t

val create : int -> 'a t

val length : 'a t -> int

val set : 'a t -> int -> 'a option -> unit

val get : 'a t -> int -> 'a option

val get_copy : 'a t -> int -> 'a option

val check : 'a t -> int -> bool

val fill : 'a t -> int -> int -> 'a option -> unit

val blit : 'a t -> int -> 'a t -> int -> int -> unit

module type S = sig
  include Weak.S

  val find_opt: t -> data -> data option
end

module Make (H : Hashtbl.HashedType) : sig
  include module type of struct
    include Weak.Make (H)
  end

  include S with type t := t and type data := data
end
