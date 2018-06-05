include module type of struct
  include Queue
end

val of_seq : 'a Stdcompat__seq.t -> 'a t

val add_seq : 'a t -> 'a Stdcompat__seq.t -> unit

val to_seq : 'a t -> 'a Stdcompat__seq.t
