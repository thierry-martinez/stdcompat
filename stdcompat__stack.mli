include module type of struct
  include Stack
end

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val of_seq : 'a Stdcompat__seq.t -> 'a t

val add_seq : 'a t -> 'a Stdcompat__seq.t -> unit

val to_seq : 'a t -> 'a Stdcompat__seq.t
