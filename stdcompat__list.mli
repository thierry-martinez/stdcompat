include module type of struct
  include List
end

val init : int -> (int -> 'a) -> 'a list

val iteri : (int -> 'a -> unit) -> 'a list -> unit

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list

val cons : 'a -> 'a list -> 'a list

val compare_lengths : 'a list -> 'a list -> int

val compare_length_with : 'a list -> int -> int

val nth_opt : 'a list -> int -> 'a option

val find_opt : ('a -> bool) -> 'a list -> 'a option

val assoc_opt : 'a -> ('a * 'b) list -> 'b option

val assq_opt : 'a -> ('a * 'b) list -> 'b option

val to_seq : 'a list -> 'a Stdcompat__seq.t

val of_seq : 'a Stdcompat__seq.t -> 'a list
