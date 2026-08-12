type t = char
external code : char -> int = "%identity"
val chr : int -> char
val escaped : char -> string
val compare : t -> t -> int
val equal : t -> t -> bool
module Ascii :
sig
  val min : char
  val max : char
  val is_valid : char -> bool
  val is_upper : char -> bool
  val is_lower : char -> bool
  val is_letter : char -> bool
  val is_alphanum : char -> bool
  val is_white : char -> bool
  val is_blank : char -> bool
  val is_graphic : char -> bool
  val is_print : char -> bool
  val is_control : char -> bool
  val is_digit : char -> bool
  val digit_to_int : char -> int
  val digit_of_int : int -> char
  val is_hex_digit : char -> bool
  val hex_digit_to_int : char -> int
  val lower_hex_digit_of_int : int -> char
  val upper_hex_digit_of_int : int -> char
  val uppercase : char -> char
  val lowercase : char -> char
end
val lowercase_ascii : char -> char
val uppercase_ascii : char -> char
val seeded_hash : int -> t -> int
val hash : t -> int
external unsafe_chr : int -> char = "%identity"
