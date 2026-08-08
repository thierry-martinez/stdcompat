val for_all :
    length:('a -> int) -> unsafe_get:('a -> int -> 'e) ->
    ('e -> bool) -> 'a -> bool

val for_all2 :
    caller:string ->
    length1:('a1 -> int) -> unsafe_get1:('a1 -> int -> 'e1) ->
    length2:('a2 -> int) -> unsafe_get2:('a2 -> int -> 'e2) ->
    ('e1 -> 'e2 -> bool) -> 'a1 -> 'a2 -> bool

val exists :
    length:('a -> int) -> unsafe_get:('a -> int -> 'e) ->
    ('e -> bool) -> 'a -> bool

val exists2 :
    caller:string ->
    length1:('a1 -> int) -> unsafe_get1:('a1 -> int -> 'e1) ->
    length2:('a2 -> int) -> unsafe_get2:('a2 -> int -> 'e2) ->
    ('e1 -> 'e2 -> bool) -> 'a1 -> 'a2 -> bool

val find_mapi :
    length:('a -> int) ->
    unsafe_get:('a -> int -> 'e) ->
    (int -> 'e -> 'b option) -> 'a -> 'b option

val find_opt :
    length:('a -> int) ->
    unsafe_get:('a -> int -> 'e) ->
    ('e -> bool) -> 'a -> 'e option

val find_index :
    length:('a -> int) ->
    unsafe_get:('a -> int -> 'e) ->
    ('e -> bool) -> 'a -> int option

val mapi_inplace :
    length:('a -> int) ->
    unsafe_get:('a -> int -> 'e) ->
    unsafe_set:('a -> int -> 'e -> unit) ->
    (int -> 'e -> 'e) -> 'a -> unit

val map_inplace :
    length:('a -> int) ->
    unsafe_get:('a -> int -> 'e) ->
    unsafe_set:('a -> int -> 'e -> unit) ->
    ('e -> 'e) -> 'a -> unit

val equal :
    length:('a -> int) -> unsafe_get:('a -> int -> 'e) ->
    ('e -> 'e -> bool) -> 'a -> 'a -> bool

val chain_compare : int -> (unit -> int) -> int

val compare :
    length:('a -> int) -> unsafe_get:('a -> int -> 'e) ->
    ('e -> 'e -> int) -> 'a -> 'a -> int
