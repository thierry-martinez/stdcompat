let rec for_all_aux ~len ~unsafe_get p a i =
  i >= len || p (unsafe_get a i) && for_all_aux ~len ~unsafe_get p a (succ i)

let for_all ~length ~unsafe_get p a =
  for_all_aux ~len:(length a) ~unsafe_get p a 0

let exists ~length ~unsafe_get p a =
  not (for_all ~length ~unsafe_get (fun e -> not (p e)) a)

let rec for_all2_aux ~len ~unsafe_get1 ~unsafe_get2 p a1 a2 i =
  i >= len ||
  p (unsafe_get1 a1 i) (unsafe_get2 a2 i)
    && for_all2_aux ~len ~unsafe_get1 ~unsafe_get2 p a1 a2 (succ i)

let for_all2 ~caller ~length1 ~unsafe_get1 ~length2 ~unsafe_get2 p a1 a2 =
  let len1 = length1 a1 in
  let len2 = length2 a2 in
  if len1 <> len2 then
    invalid_arg caller;
  for_all2_aux ~len:len1 ~unsafe_get1 ~unsafe_get2 p a1 a2 0

let exists2 ~caller ~length1 ~unsafe_get1 ~length2 ~unsafe_get2 p a1 a2 =
  not (for_all2 ~caller ~length1 ~unsafe_get1 ~length2 ~unsafe_get2
    (fun e1 e2 -> not (p e1 e2)) a1 a2)

let rec find_mapi_aux ~len ~unsafe_get f a i =
  if i >= len then
    None
  else
    match f i (unsafe_get a i) with
    | Some _ as some -> some
    | None -> find_mapi_aux ~len ~unsafe_get f a (succ i)

let find_mapi ~length ~unsafe_get f a =
  find_mapi_aux ~len:(length a) ~unsafe_get f a 0

let find_opt ~length ~unsafe_get p a =
  find_mapi ~length ~unsafe_get (fun _i v -> if p v then Some v else None) a

let find_index ~length ~unsafe_get p a =
  find_mapi ~length ~unsafe_get (fun i v -> if p v then Some i else None) a

let mapi_inplace ~length ~unsafe_get ~unsafe_set f a =
  for i = 0 to length a - 1 do
    unsafe_set a i (f i (unsafe_get a i))
  done

let map_inplace ~length ~unsafe_get ~unsafe_set f a =
  mapi_inplace ~length ~unsafe_get ~unsafe_set (fun _i v -> f v) a

let equal ~length ~unsafe_get p a1 a2 =
  let len = length a1 in
  len = length a2 &&
  for_all2_aux ~len ~unsafe_get1:unsafe_get ~unsafe_get2:unsafe_get p a1 a2 0

let chain_compare cmp f =
  if cmp = 0 then
    f ()
  else
    cmp

let rec compare_aux ~len ~unsafe_get cmp a1 a2 i =
  if i >= len then
    0
  else
    chain_compare (cmp (unsafe_get a1 i) (unsafe_get a2 i)) (fun () ->
      compare_aux ~len ~unsafe_get cmp a1 a2 (succ i))

let compare ~length ~unsafe_get cmp a1 a2 =
  let len = length a1 in
  chain_compare (Int.compare len (length a2)) (fun () ->
    compare_aux ~len ~unsafe_get cmp a1 a2 0) 
