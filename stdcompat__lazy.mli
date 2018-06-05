include module type of Lazy

val from_fun : (unit -> 'a) -> 'a t

val from_val : 'a -> 'a t