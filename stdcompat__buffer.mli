include module type of struct
  include Buffer
end

val to_bytes : t -> Stdcompat__root.bytes

val truncate : t -> int -> unit
(** @before 4.05.0 duplicates the prefix kept. *)

val add_bytes : t -> Stdcompat__root.bytes -> unit

val add_subbytes : t -> Stdcompat__root.bytes -> int -> int -> unit

val add_utf_8_uchar : t -> Stdcompat__uchar.t -> unit

val add_utf_16be_uchar : t -> Stdcompat__uchar.t -> unit

val add_utf_16le_uchar : t -> Stdcompat__uchar.t -> unit

val of_seq : char Stdcompat__seq.t -> t

val add_seq : t -> char Stdcompat__seq.t -> unit

val to_seq : t -> char Stdcompat__seq.t

val to_seqi : t -> (int * char) Stdcompat__seq.t
