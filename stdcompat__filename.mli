include module type of struct
  include Filename
end

val extension : string -> string

val remove_extension : string -> string

val get_temp_dir_name : unit -> string

val set_temp_dir_name : string -> unit

val open_temp_file :
    ?mode : open_flag list -> ?perms : int -> ?temp_dir : string -> string
        -> string -> string * out_channel
