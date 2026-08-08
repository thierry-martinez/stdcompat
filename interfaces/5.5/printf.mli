val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val ifprintf : 'b -> ('a, 'b, 'c, unit) format4 -> 'a
val ibprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val kfprintf :
  (out_channel -> 'd) ->
    out_channel -> ('a, out_channel, unit, 'd) format4 -> 'a
val ikfprintf : ('b -> 'd) -> 'b -> ('a, 'b, 'c, 'd) format4 -> 'a
val ksprintf : (string -> 'd) -> ('a, unit, string, 'd) format4 -> 'a
val kbprintf :
  (Buffer.t -> 'd) -> Buffer.t -> ('a, Buffer.t, unit, 'd) format4 -> 'a
val ikbprintf :
  (Buffer.t -> 'd) -> Buffer.t -> ('a, Buffer.t, unit, 'd) format4 -> 'a
module Args :
sig
  type ('a, 'r) t =
    | []: ('r, 'r) t 
    | (::): 'a * ('b, 'r) t -> ('a -> 'b, 'r) t 
  val apply : 'a -> ('a, 'r) t -> 'r
  val (@) : ('a, 'r1) t -> ('r1, 'r2) t -> ('a, 'r2) t
end
val lfprintf :
  out_channel -> ('a, out_channel, unit) format -> ('a, unit) Args.t -> unit
val lbprintf :
  Buffer.t -> ('a, Buffer.t, unit) format -> ('a, unit) Args.t -> unit
val lprintf : ('a, out_channel, unit) format -> ('a, unit) Args.t -> unit
val leprintf : ('a, out_channel, unit) format -> ('a, unit) Args.t -> unit
val lsprintf : ('a, unit, string) format -> ('a, string) Args.t -> string
val kprintf : (string -> 'b) -> ('a, unit, string, 'b) format4 -> 'a
