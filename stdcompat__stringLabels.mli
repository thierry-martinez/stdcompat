include module type of StringLabels

val init : int -> f:(int -> char) -> string

val mapi : f:(int -> char -> char) -> string -> string

val iteri : f:(int -> char -> unit) -> string -> unit

val map : f:(char -> char) -> string -> string

val trim : string -> string

val lowercase_ascii : string -> string

val uppercase_ascii : string -> string

val capitalize_ascii : string -> string

val uncapitalize_ascii : string -> string

val equal : t -> t -> bool

val split_on_char : sep:char -> string -> string list

val index_opt : string -> char -> int option

val rindex_opt : string -> char -> int option

val index_from_opt : string -> int -> char -> int option

val rindex_from_opt : string -> int -> char -> int option

val of_seq : char Stdcompat__seq.t -> t

val to_seq : t -> char Stdcompat__seq.t

val to_seqi : t -> (int * char) Stdcompat__seq.t
