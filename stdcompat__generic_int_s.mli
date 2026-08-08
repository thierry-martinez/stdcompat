module type S = sig
  type t

  val bitsize : int

  val zero : t

  val one : t

  val neg : t -> t

  val pred : t -> t

  val succ : t -> t

  val sub : t -> t -> t

  val div : t -> t -> t

  val rem : t -> t -> t

  val lognot : t -> t

  val logand : t -> t -> t

  val shift_right_logical : t -> int -> t
end

module type F = sig
  type t

  val fdiv : t -> t -> t

  val cdiv : t -> t -> t

  val ediv : t -> t -> t

  val erem : t -> t -> t

  val popcount : t -> int

  val unsigned_bitsize : t -> int

  val signed_bitsize : t -> int

  val leading_zeros : t -> int

  val leading_sign_bits : t -> int

  val trailing_zeros : t -> int
end
