let () =
  assert (Stdcompat.hypot 3. 4. = 5.);
  assert (Stdcompat.copysign 1. 2. = 1.);
  assert (Stdcompat.copysign 1. (-. 2.) = -. 1.);
  assert (Stdcompat.copysign (-. 1.) 2. = 1.);
  assert (Stdcompat.copysign (-. 1.) (-. 2.) = -. 1.);
  assert (
    try
      ignore (Stdcompat.raise_notrace Exit);
      false
    with Exit -> true);
  assert (Stdcompat.bool_of_string_opt "true" = Some true);
  assert (Stdcompat.bool_of_string_opt "false" = Some false);
  assert (Stdcompat.bool_of_string_opt "foo" = None);
  assert (Stdcompat.int_of_string_opt "42" = Some 42);
  assert (Stdcompat.int_of_string_opt "foo" = None);
  assert (Stdcompat.float_of_string_opt "42." = Some 42.);
  assert (Stdcompat.float_of_string_opt "foo" = None);
  assert (Lazy.force (Stdcompat.Lazy.from_fun (fun () -> 42)) = 42);
  assert (Lazy.force (Stdcompat.Lazy.from_val 42) = 42);
  assert (Stdcompat.Char.lowercase_ascii 'A' = 'a');
  assert (Stdcompat.Char.uppercase_ascii 'a' = 'A');
  assert (Stdcompat.Char.equal 'a' 'a');
  assert (not (Stdcompat.Char.equal 'A' 'a'));
  assert (Stdcompat.String.init 2 (fun i -> char_of_int i) = "\000\001");
  assert
    (Stdcompat.String.mapi
       (fun i c -> char_of_int (i + int_of_char c)) "abc" = "ace");
  assert (
    let s = Stdcompat.Bytes.create 3 in
    Stdcompat.String.iteri (fun i c -> Stdcompat.Bytes.set s i c) "abc";
    s = Stdcompat.Bytes.of_string "abc");
  assert (Stdcompat.String.map Stdcompat.Char.uppercase_ascii "abc" = "ABC");
  assert (Stdcompat.String.trim " \t abc\n" = "abc");
  assert (Stdcompat.String.lowercase_ascii "AbcD" = "abcd");
  assert (Stdcompat.String.uppercase_ascii "AbcD" = "ABCD");
  assert (Stdcompat.String.capitalize_ascii "abcD" = "AbcD");
  assert (Stdcompat.String.uncapitalize_ascii "AbcD" = "abcD");
  assert (Stdcompat.String.equal "abc" "abc");
  assert (not (Stdcompat.String.equal "Abc" "abc"));
  assert (Stdcompat.String.split_on_char ' ' " abc  d ef  "
    = ["";"abc";"";"d";"ef";"";""]);
  assert (Stdcompat.String.index_opt "abaababa" 'a' = Some 0);
  assert (Stdcompat.String.index_opt "abaababa" 'c' = None);
  assert (Stdcompat.String.rindex_opt "abaababa" 'a' = Some 7);
  assert (Stdcompat.String.rindex_opt "abaababa" 'c' = None);
  assert (Stdcompat.String.index_from_opt "abaababa" 1 'a' = Some 2);
  assert (Stdcompat.String.index_from_opt "abaababa" 1 'c' = None);
  assert (Stdcompat.String.rindex_from_opt "abaababa" 4 'a' = Some 3);
  assert (Stdcompat.String.rindex_from_opt "abaababa" 4 'c' = None);
  assert (
    let s = Stack.create () in
    Stack.push 1 s;
    Stack.push 2 s;
    Stack.push 3 s;
    Stdcompat.Stack.fold ( + ) 0 s = 6 &&
    Stack.length s = 3);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 2;
    Hashtbl.add t 3 4;
    (Stdcompat.Hashtbl.stats t).Stdcompat.Hashtbl.num_bindings = 2);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 1;
    Hashtbl.add t 2 2;
    Hashtbl.add t 2 3;
    Hashtbl.add t 3 4;
    Stdcompat.Hashtbl.filter_map_inplace
        (fun k v -> if k = 3 then None else Some (pred v)) t;
    Hashtbl.find_all t 1 = [0] &&
    Hashtbl.find_all t 2 = [2; 1] &&
    Hashtbl.find_all t 3 = []);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 1;
    Hashtbl.add t 2 2;
    Stdcompat.Hashtbl.find_opt t 1 = Some 1 &&
    Stdcompat.Hashtbl.find_opt t 3 = None);
  assert (
    let module H = struct
      type t = int

      let equal : int -> int -> bool = ( = )

      let hash : int -> int = fun x -> x
    end in
    let module M = Hashtbl.Make (H) in
    let t = M.create 17 in
    M.add t 1 1;
    M.add t 2 2;
    let module M = Stdcompat.Hashtbl.Make (H) in
    M.find_opt t 1 = Some 1 &&
    M.find_opt t 3 = None);
  assert (
    let l = ref [] in
    let f a b =
      l := (a, b) :: !l in
    Stdcompat.List.iteri f [1; 2; 3];
    !l = [2, 3; 1, 2; 0, 1]);
  assert (
    let f a b =
      (a, b) in
    Stdcompat.List.mapi f  [1; 2; 3] = [0, 1; 1, 2; 2, 3]);
  assert (
    Stdcompat.List.sort_uniq compare  [2; 1; 3; 2; 1; 3]
    = [1; 2; 3]);
  assert (Stdcompat.List.cons 1 [2; 3] = [1; 2; 3]);
  assert (Stdcompat.List.compare_lengths [1] [2; 3] < 0);
  assert (Stdcompat.List.compare_lengths [1; 2] [2; 3] = 0);
  assert (Stdcompat.List.compare_lengths [1; 2; 3] [2; 3] > 0);
  assert (Stdcompat.List.compare_length_with [1] 2 < 0);
  assert (Stdcompat.List.compare_length_with [1; 2] 2 = 0);
  assert (Stdcompat.List.compare_length_with [1; 2; 3] 2 > 0);;
  assert (Stdcompat.List.nth_opt [1; 2; 3] 2 = Some 3);
  assert (Stdcompat.List.nth_opt [1; 2; 3] 3 = None);
  assert (
    try
      ignore (Stdcompat.List.nth_opt [1; 2; 3] (-1));
      false
    with Invalid_argument _ -> true);
  assert (Stdcompat.List.find_opt (fun i -> i mod 2 = 0) [1; 2; 3] = Some 2);
  assert (Stdcompat.List.find_opt (fun i -> i mod 4 = 0) [1; 2; 3] = None);
  assert (Stdcompat.List.assoc_opt 2 [1, 0; 2, 1; 3, 2] = Some 1);
  assert (Stdcompat.List.assoc_opt 4 [1, 0; 2, 1; 3, 2] = None);
  assert (Stdcompat.List.assq_opt 2 [1, 0; 2, 1; 3, 2] = Some 1);
  assert (Stdcompat.List.assq_opt "a" ["a", 1; "b", 2; "c", 3] = None);
  assert (Stdcompat.Filename.extension "a.b/c.de" = ".de");
  assert (Stdcompat.Filename.extension "a.b/cd" = "");
  assert (Stdcompat.Filename.remove_extension "a.b/c.de" = "a.b/c");
  assert (Stdcompat.Filename.remove_extension "a.b/cd" = "a.b/cd");
  assert (
    let array = Stdcompat.Array.Floatarray.create 2 in
    Stdcompat.Array.Floatarray.set array 0 1.;
    Stdcompat.Array.Floatarray.set array 1 2.;
    Stdcompat.Array.Floatarray.get array 0 = 1. &&
    Stdcompat.Array.Floatarray.get array 1 = 2.);
  assert (
    let l = ref [] in
    let f a b =
      l := (a, b) :: !l in
    Stdcompat.Array.iter2 f [| 0; 1 |] [| 2; 3 |];
    !l = [1, 3; 0, 2]);
  assert (
    let f a b =
      (a, b) in
    Stdcompat.Array.map2 f  [| 0; 1 |] [| 2; 3 |] = [| 0, 2; 1, 3 |]);
  assert (Stdcompat.Array.for_all (fun x -> x > 0) [| 1; 2; 3 |]);
  assert (not (Stdcompat.Array.for_all (fun x -> x > 0) [| 1; 2; 0; 3 |]));
  assert (Stdcompat.Array.exists (fun x -> x > 2) [| 1; 2; 3 |]);
  assert (not (Stdcompat.Array.exists (fun x -> x > 3) [| 1; 2; 3 |]));
  assert (Stdcompat.Array.mem "a" [| "a"; "b"; "c" |]);
  assert (not (Stdcompat.Array.mem "d" [| "a"; "b"; "c" |]));
  assert (Stdcompat.Array.memq 2 [| 1; 2; 3 |]);
  assert (not (Stdcompat.Array.memq "a" [| "a"; "b"; "c" |]))
