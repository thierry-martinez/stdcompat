include module type of struct
  include ListLabels
end

val init : len:int -> f:(int -> 'a) -> 'a list

val iteri : f:(int -> 'a -> unit) -> 'a list -> unit

val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list

val sort_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list

val cons : 'a -> 'a list -> 'a list

val compare_lengths : 'a list -> 'a list -> int

val compare_length_with : 'a list -> len:int -> int

val nth_opt : 'a list -> int -> 'a option

val find_opt : f:('a -> bool) -> 'a list -> 'a option

val assoc_opt : 'a -> ('a * 'b) list -> 'b option

val assq_opt : 'a -> ('a * 'b) list -> 'b option

val to_seq : 'a list -> 'a Stdcompat__seq.t

val of_seq : 'a Stdcompat__seq.t -> 'a list
