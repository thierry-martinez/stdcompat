include module type of struct
  include Int64
end

val equal : t -> t -> bool

val of_string_opt : string -> t option
