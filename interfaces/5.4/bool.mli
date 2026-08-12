type t = bool =
  | false 
  | true 
val not : bool -> bool
external (&&) : bool -> bool -> bool = "%sequand"
external (||) : bool -> bool -> bool = "%sequor"
val logand : bool -> bool -> bool
val logor : bool -> bool -> bool
val logxor : bool -> bool -> bool
val equal : bool -> bool -> bool
val compare : bool -> bool -> int
val to_int : bool -> int
val to_float : bool -> float
val to_string : bool -> string
val seeded_hash : int -> bool -> int
val hash : bool -> int
