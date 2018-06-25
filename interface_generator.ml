module Option = struct
  type 'a t = 'a option

  let map f o =
    match o with
    | None -> None
    | Some x -> Some (f x)

  let equal p o o' =
    match o, o' with
    | None, None -> true
    | Some x, Some y -> p x y
    | Some _, None
    | None, Some _ -> false

  let filter p o =
    match o with
    | None -> false
    | Some x -> p x

  let some x = Some x
end

module String = struct
  include String

  let hash = Hashtbl.hash

  module Map = Map.Make (String)
end

module List = struct
  include List

  let filter_map f =
    let rec find accu l =
      match l with
      | [] -> rev accu
      | x :: l ->
          match f x with
          | None -> find accu l
          | Some y -> find (y :: accu) l in
    find []

  let equal p l l' =
    List.compare_lengths l l' = 0 && List.for_all2 p l l'

  let rec last l =
    match l with
    | [] -> failwith "last"
    | [item] -> item
    | _ :: tl -> last tl
end

module Name = struct
  type t = string Location.loc

  let equal (n : t) (n' : t) =
    n.txt = n'.txt
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b

  let map_snd f (x, y) =
    (x, f y)

  let with_snd f (_x, y) =
    f y

  module Hashed (X : Hashtbl.HashedType) (Y : Hashtbl.HashedType) = struct
    type t = X.t * Y.t

    let equal (x, y) (x', y') =
      X.equal x x' && Y.equal y y'

    let hash (x, y) =
      Hashtbl.hash (X.hash x, Y.hash y)
  end
end

type isomorphism_kind = Ignore_attributes | Attributes_equal

let is_attribute_isomorphic kind (id, payload) (id', payload') =
  Name.equal id id'

let is_attributes_isomorphic kind (a : Parsetree.attributes)
    (a' : Parsetree.attributes) =
  match kind with
  | Ignore_attributes -> true
  | Attributes_equal ->
      List.equal (is_attribute_isomorphic kind) a a'

let rec is_core_type_isomorphic kind (t : Parsetree.core_type)
    (t' : Parsetree.core_type) =
  is_attributes_isomorphic kind t.ptyp_attributes t'.ptyp_attributes &&
  match t.ptyp_desc, t'.ptyp_desc with
  | Ptyp_any, Ptyp_any -> true
  | Ptyp_var v, Ptyp_var v' -> v = v'
  | Ptyp_arrow (l, u, v), Ptyp_arrow (l', u', v') ->
      l = l' &&
      is_core_type_isomorphic kind u u' &&
      is_core_type_isomorphic kind v v'
  | Ptyp_tuple l, Ptyp_tuple l' ->
      List.equal (is_core_type_isomorphic kind) l l'
  | Ptyp_constr (c, args), Ptyp_constr (c', args') ->
      c.txt = c'.txt && List.equal (is_core_type_isomorphic kind) args args'
  | Ptyp_alias (c, v), Ptyp_alias (c', v') ->
      is_core_type_isomorphic kind c c' && v = v'
  | Ptyp_object (l, flag), Ptyp_object (l', flag') ->
      List.equal (is_object_field_isomorphic kind) l l' && flag = flag'
  | Ptyp_poly (l, t), Ptyp_poly (l', t') ->
      List.equal Name.equal l l' && is_core_type_isomorphic kind t t'
  | Ptyp_variant _, _ ->
      Format.fprintf Format.err_formatter "Unknown isomorphism variant: %a@." Pprintast.core_type t;
      false
  | _ ->
      Format.fprintf Format.err_formatter "Unknown isomorphism: %a@." Pprintast.core_type t;
      false

and is_object_field_isomorphic kind (f : Parsetree.object_field)
    (f' : Parsetree.object_field) =
  match f, f' with
  | Otag (l, a, t), Otag (l', a', t') ->
      l.txt = l'.txt &&
      is_attributes_isomorphic kind a a' &&
      is_core_type_isomorphic kind t t'
  | Oinherit t, Oinherit t' ->
      is_core_type_isomorphic kind t t'
  | _ -> false

let is_label_declaration_isomorphic kind (l : Parsetree.label_declaration)
    (l' : Parsetree.label_declaration) =
  Name.equal l.pld_name l'.pld_name &&
  l.pld_mutable = l'.pld_mutable &&
  is_core_type_isomorphic kind l.pld_type l'.pld_type

let is_constructor_arguments_isomorphic kind
    (a : Parsetree.constructor_arguments)
    (a' : Parsetree.constructor_arguments) =
  match a, a' with
  | Pcstr_tuple l, Pcstr_tuple l' ->
      List.equal (is_core_type_isomorphic kind) l l'
  | Pcstr_record l, Pcstr_record l' ->
      List.equal (is_label_declaration_isomorphic kind) l l'
  | _ -> false

let is_constructor_declaration_isomorphic kind
    (c : Parsetree.constructor_declaration)
    (c' : Parsetree.constructor_declaration) =
  Name.equal c.pcd_name c'.pcd_name &&
  is_attributes_isomorphic kind c.pcd_attributes c'.pcd_attributes &&
  is_constructor_arguments_isomorphic kind c.pcd_args c'.pcd_args &&
  Option.equal (is_core_type_isomorphic kind) c.pcd_res c'.pcd_res

let no_self_manifest (d : Parsetree.type_declaration) =
  match d.ptype_manifest with
  | Some ({ ptyp_desc = Ptyp_constr ({ txt = Lident self }, _args)})
    when self = d.ptype_name.txt -> None
  | manifest -> manifest

let is_type_declaration_isomorphic kind (d : Parsetree.type_declaration)
    (d' : Parsetree.type_declaration) =
  Name.equal d.ptype_name d'.ptype_name &&
  is_attributes_isomorphic kind d.ptype_attributes d'.ptype_attributes &&
  Option.equal (is_core_type_isomorphic kind)
    (no_self_manifest d) (no_self_manifest d') &&
  match d.ptype_kind, d'.ptype_kind with
  | Ptype_abstract, Ptype_abstract
  | Ptype_open, Ptype_open ->
      true
  | Ptype_variant c, Ptype_variant c' ->
      List.equal (is_constructor_declaration_isomorphic kind) c c'
  | Ptype_record r, Ptype_record r' ->
      List.equal (is_label_declaration_isomorphic kind) r r'
  | _ -> false

module Type_declaration_block = struct
  type t = {
      rec_flag : Asttypes.rec_flag;
      type_decl : Parsetree.type_declaration list;
    }

  let get_first_type_name b =
    match b.type_decl with
    | [] -> failwith "empty type decl"
    | type_decl :: _ -> type_decl.ptype_name.txt

  let is_isomorphic kind b b' =
    b.rec_flag = b'.rec_flag &&
    List.equal (is_type_declaration_isomorphic kind) b.type_decl b'.type_decl
end

module Signature = struct
  type t = {
      values : Parsetree.value_description String.Map.t;
      types : Type_declaration_block.t String.Map.t;
      module_types : Parsetree.module_type_declaration String.Map.t;
      modules : Parsetree.module_declaration String.Map.t;
    }

  let empty = {
    values = String.Map.empty;
    types = String.Map.empty;
    module_types = String.Map.empty;
    modules = String.Map.empty;
  }

  let add_value (value_desc : Parsetree.value_description) s =
    let values = String.Map.add value_desc.pval_name.txt value_desc s.values in
    { s with values }

  let add_type_declaration (type_decl : Parsetree.type_declaration) block s =
    let types = String.Map.add type_decl.ptype_name.txt block s.types in
    { s with types }

  let add_type_declaration_block (block : Type_declaration_block.t) s =
    block.type_decl |>
      List.fold_left (fun s decl -> add_type_declaration decl block s) s

  let add_module_type_declaration
      (module_type_declaration : Parsetree.module_type_declaration)
      s =
    let module_types =
      String.Map.add module_type_declaration.pmtd_name.txt
        module_type_declaration s.module_types in
    { s with module_types }

  let add_module_declaration (module_declaration : Parsetree.module_declaration)
      s =
    let modules =
      String.Map.add module_declaration.pmd_name.txt module_declaration
        s.modules in
    { s with modules }

  let add_item (item : Parsetree.signature_item) s =
    match item.psig_desc with
    | Psig_value value_desc -> add_value value_desc s
    | Psig_type (rec_flag, type_decl) ->
        add_type_declaration_block { rec_flag; type_decl } s
    | Psig_modtype module_type_declaration ->
        add_module_type_declaration module_type_declaration s
    | Psig_module module_declaration ->
        add_module_declaration module_declaration s
    | _ -> s

  let of_parsetree s =
    s |> List.fold_left (fun s item -> add_item item s) empty
end

let try_close f ~close =
  match f () with
  | result ->
      close ();
      result
  | exception e ->
      begin
        try
          close ()
        with _ ->
          ()
      end;
      e |> raise

module Version = struct
  type t = {
      major : int;
      minor : int;
      patch : int;
    }

  let compare (v : t) (v' : t) =
    compare v v'

  let equal (v : t) (v' : t) =
    v = v'

  let hash (v : t) =
    Hashtbl.hash v

  let of_version_line version_line =
    let version_line_length = String.length version_line in
    let version_last_index =
      match String.rindex version_line '+' with
      | exception Not_found -> version_line_length
      | index ->
          match String.rindex version_line ' ' with
          | exception Not_found -> assert false
          | space_index ->
              if space_index < index then
                index
              else
                version_line_length in
    let major =
      String.sub version_line (version_last_index - 6) 1 |> int_of_string in
    let minor =
      String.sub version_line (version_last_index - 4) 2 |> int_of_string in
    let patch =
      0
(* String.sub version_line (version_last_index - 1) 1 |> int_of_string *) in
    { major; minor; patch }

  let of_command_line command_line =
    let version_command_line = Printf.sprintf "%s -version" command_line in
    let in_channel = Unix.open_process_in version_command_line in
    let version_line =
      try_close
        ~close:(fun () ->
          assert (in_channel |> Unix.close_process_in = Unix.WEXITED 0))
      @@ fun () -> input_line in_channel in
    of_version_line version_line

  let to_string ?(sep = ".") { major; minor; patch } =
    Printf.sprintf "%d%s%.2d%s%d" major sep minor sep patch
end

type interpreter = {
    command_line : string;
    version : Version.t;
  }

let ask_interface ~interpreter ~module_name =
  let channels =
    Unix.open_process_full interpreter.command_line (Unix.environment ()) in
  try_close
    ~close:(fun () ->
      assert (channels |> Unix.close_process_full = Unix.WEXITED 0))
    @@ fun () ->
      let in_channel, out_channel, err_channel = channels in
      Unix.sleepf 0.1;
      let bytes_size = 1024 in
      let bytes = Bytes.create bytes_size in
      let _ : int = input in_channel bytes 0 bytes_size in
      let top_command =
        if Version.compare interpreter.version
            { major = 4; minor = 02; patch = 0 } >= 0 then
          Printf.sprintf "module type M = module type of %s;;\nexit 0;;\n"
            module_name
        else
          Printf.sprintf "module M = %s;;\nexit 0;;\n" module_name in
      output_string out_channel top_command;
      flush out_channel;
      let buffer = Buffer.create bytes_size in
      let rec loop () =
        match input in_channel bytes 0 bytes_size with
        | 0 -> ()
        | nb_read ->
            Buffer.add_subbytes buffer bytes 0 nb_read;
            loop () in
      loop ();
      Buffer.truncate buffer (Buffer.length buffer - 2);
      if Buffer.sub buffer 0 2 = "# " then
        Buffer.sub buffer 2 (Buffer.length buffer - 2)
      else
        Buffer.contents buffer

let lexbuf_set_filename (lexbuf : Lexing.lexbuf) filename =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = filename }

let signature_of_lexbuf mli_filename lexbuf =
  lexbuf_set_filename lexbuf mli_filename;
  lexbuf |> Parse.interface

let signature_of_in_channel mli_filename in_channel =
  let buf = in_channel |> Lexing.from_channel in
  buf.lex_curr_p <- { buf.lex_curr_p with Lexing.pos_fname = mli_filename };
  buf |> Parse.interface

let read_interface_file mli_filename =
  let in_channel = mli_filename |> open_in in
  try_close
    ~close:(fun () -> in_channel |> close_in)
    @@ fun () ->
      let buf = in_channel |> Lexing.from_channel in
      buf.lex_curr_p <- { buf.lex_curr_p with Lexing.pos_fname = mli_filename };
      buf |> Parse.interface

module SignaturesTable = Hashtbl.Make (Pair.Hashed (Version) (String))

let signatures = SignaturesTable.create 17

let module_type_of_name ~interpreter ~module_name =
  try
    SignaturesTable.find signatures (interpreter.version, module_name)
  with Not_found ->
    let s = ask_interface ~interpreter ~module_name in
    let signature =
      match
        match signature_of_lexbuf module_name (s |> Lexing.from_string) with
        | [{ psig_desc =
             Psig_module
               { pmd_type = module_type } }] ->
                 module_type
        | [{ psig_desc =
             Psig_modtype
               { pmtd_type = Some module_type }}] ->
                 module_type
        | _ -> failwith "Unexpected result"
      with
      | exception Syntaxerr.Error(err) ->
          prerr_endline s;
          Syntaxerr.report_error Format.err_formatter err;
          { pmty_desc = Pmty_signature []; pmty_loc = Location.none;
            pmty_attributes = [] }
      | exception e ->
          prerr_endline s;
          prerr_endline (Printexc.to_string e);
          { pmty_desc = Pmty_signature []; pmty_loc = Location.none;
            pmty_attributes = [] }
      | s ->
          s in
    SignaturesTable.add signatures (interpreter.version, module_name) signature;
    signature

module VersionHashtbl = Hashtbl.Make (Version)

let pervasives_externals = VersionHashtbl.create 17

module StringHashtbl = Hashtbl.Make (String)

let signature_of_module_type (modtype : Parsetree.module_type) =
  match modtype.pmty_desc with
  | Pmty_signature s -> s
  | _ -> failwith "signature_of_module_type"

let get_pervasives_externals interpreter =
  try
    VersionHashtbl.find pervasives_externals interpreter.version
  with Not_found ->
    let modtype = module_type_of_name interpreter "Pervasives" in
    let externals = StringHashtbl.create 17 in
    let add_external (item : Parsetree.signature_item) =
      match item.psig_desc with
      | Psig_value ({ pval_prim = prim :: _ } as value_description) ->
          StringHashtbl.add externals prim value_description
      | _ -> () in
    modtype |> signature_of_module_type |> List.iter add_external;
    VersionHashtbl.add pervasives_externals interpreter.version externals;
    externals

let string_of_longident longident =
  String.concat "." (Longident.flatten longident)

let module_type_of_longident ~interpreter longident =
  let module_name = string_of_longident longident in
  module_type_of_name ~interpreter ~module_name

let qualify_type_decl ~module_name (type_decl : Parsetree.type_declaration) =
  let ptype_manifest =
    type_decl.ptype_manifest |> Option.map @@ fun (ty : Parsetree.core_type) ->
      match ty.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "fpclass"; loc }, []) ->
          let txt = Longident.Ldot (Lident "Pervasives", "fpclass") in
          let ptyp_desc = Parsetree.Ptyp_constr ({ Location.txt; loc }, []) in
          { ty with ptyp_desc }
      | Ptyp_constr ({ txt = Lident ident; loc }, args)
        when ident <> "char" && ident <> "string" && ident <> "lazy_t"
            && ident <> "nativeint" && ident <> "int32" && ident <> "int64"
            && ident <> "format6" && ident <> "bytes" && ident <> "float" ->
          let txt = Longident.Ldot (module_name, ident) in
          let ptyp_desc = Parsetree.Ptyp_constr ({ Location.txt; loc }, args) in
          { ty with ptyp_desc }
      | _ -> ty in
  { type_decl with ptype_manifest }

let format_signature_item ~module_name ~signatures formatter
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, type_decl) ->
      let item =
        { item with psig_desc = Parsetree.Psig_type (rec_flag, type_decl) } in
      Format.fprintf formatter "%a@." Pprintast.signature [item]
  | Psig_value value_desc ->
      ()
  | _ ->
      ()

let rec compat_core_type ~module_name (core_type : Parsetree.core_type) =
  let core_type = { core_type with ptyp_attributes = [] } in
  match core_type.ptyp_desc with
  | Ptyp_arrow (label, left, right) ->
      let ptyp_desc =
        Parsetree.Ptyp_arrow
          (label, compat_core_type ~module_name left,
           compat_core_type ~module_name right) in
      { core_type with ptyp_desc }
  | Ptyp_tuple args ->
      let ptyp_desc =
        Parsetree.Ptyp_tuple (List.map (compat_core_type ~module_name) args) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Lident "bytes" }, []) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__init", "bytes") }, []) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Lident "floatarray" }, []) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__init", "floatarray") }, []) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "Seq", "t") }, [arg]) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__seq", "t") },
           [compat_core_type ~module_name arg]) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "Uchar", "t") }, []) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__uchar", "t") }, []) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "Hashtbl", "statistics") }, []) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat_hashtbl_ext", "statistics") },
           []) in
      { core_type with ptyp_desc }
  | Ptyp_constr (constr, args) ->
      let rec remove_module_name (constr : Longident.t) : Longident.t =
        match constr with
        | Ldot (module_name', x) ->
            if module_name = module_name' then
              Lident x
            else
              Ldot (remove_module_name module_name', x)
        | _ -> constr in
      let constr = { constr with txt = remove_module_name constr.txt } in
      let ptyp_desc =
        Parsetree.Ptyp_constr
          (constr,
           List.map (compat_core_type ~module_name) args) in
      { core_type with ptyp_desc }
  | _ -> core_type

let compat_label_declaration ~module_name
    (label_declaration : Parsetree.label_declaration) =
  { label_declaration with
    pld_type = compat_core_type ~module_name label_declaration.pld_type }

let compat_constructor_arguments ~module_name
    (constructor_arguments : Parsetree.constructor_arguments) :
    Parsetree.constructor_arguments =
  match constructor_arguments with
  | Pcstr_tuple args ->
      Pcstr_tuple (args |> List.map (compat_core_type ~module_name))
  | Pcstr_record label_declarations ->
      Pcstr_record (label_declarations |>
      List.map (compat_label_declaration ~module_name))

let compat_constructor_declaration ~module_name
    (constructor_declaration : Parsetree.constructor_declaration) =
  { constructor_declaration with
    pcd_args =
    compat_constructor_arguments ~module_name constructor_declaration.pcd_args;
    pcd_res = constructor_declaration.pcd_res |>
    Option.map (compat_core_type ~module_name);
  }

let compat_type_kind ~module_name
    (type_kind : Parsetree.type_kind) : Parsetree.type_kind =
  match type_kind with
  | Ptype_variant constructor_declarations ->
      Ptype_variant
        (constructor_declarations |>
        List.map (compat_constructor_declaration ~module_name))
  | Ptype_record label_declarations ->
      Ptype_record
        (label_declarations |> List.map (compat_label_declaration ~module_name))
  | _ -> type_kind

let compat_type_declaration ~module_name
    (type_decl : Parsetree.type_declaration) =
  if type_decl.ptype_name.txt = "format6" then
    let ptype_manifest =
      match type_decl.ptype_manifest with
      |  Some (
        { ptyp_desc = Parsetree.Ptyp_constr ({ loc }, args) } as core_type) ->
          let ptyp_desc =
            Parsetree.Ptyp_constr
            ({ loc;
               txt = Ldot (Lident "Stdcompat__init", "format6") }, args) in
          Some { core_type with ptyp_desc }
      | _ -> assert false in
    { type_decl with ptype_manifest }
  else
(*
    match type_decl.ptype_manifest with
    |  Some (
      { ptyp_desc =
        Parsetree.Ptyp_constr ({ loc; txt = Lident "bytes" }, args) }
        as core_type) ->
          let ptyp_desc =
            Parsetree.Ptyp_constr
            ({ loc;
               txt = Ldot (Lident "Stdcompat__init", "bytes") }, args) in
          let ptype_manifest = Some { core_type with ptyp_desc } in
          { type_decl with ptype_manifest }
    | _ ->*)
    let ptype_manifest =
      match type_decl.ptype_manifest with
      | None -> None
      | Some { ptyp_desc = Parsetree.Ptyp_constr ({ txt = Lident name }, _) }
        when name = type_decl.ptype_name.txt -> None
      | Some { ptyp_desc =
          Parsetree.Ptyp_constr
            ({ txt = Ldot (module_name', name) }, _) }
        when name = type_decl.ptype_name.txt && module_name = module_name' ->
          None
      |  Some (
        { ptyp_desc =
          Parsetree.Ptyp_constr ({ loc; txt = Lident "bytes" }, args) }
        as core_type) ->
          let ptyp_desc =
            Parsetree.Ptyp_constr
            ({ loc;
               txt = Ldot (Lident "Stdcompat__init", "bytes") }, args) in
          Some { core_type with ptyp_desc }
      | Some ptype_manifest ->
          Some (compat_core_type ~module_name ptype_manifest) in
    { type_decl with
      ptype_manifest;
      ptype_kind = compat_type_kind ~module_name type_decl.ptype_kind
    }

let compat_prim prim =
  match prim with
  | ["caml_create_string"]
  | ["%string_safe_set"]
  | ["%string_unsafe_set"]
  | ["%identity"] -> prim
  | ["caml_blit_string"]
  | ["caml_fill_string"] -> prim @ ["noalloc"]
  | ["%raise_notrace"] -> ["%raise"]
  | _ -> []

let rec compat_signature_item ~module_name ~reference_interpreter
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, type_decl) ->
      let type_decl =
        type_decl |> List.map (compat_type_declaration ~module_name) in
      let item =
        { item with psig_desc = Parsetree.Psig_type (rec_flag, type_decl) } in
      item
  | Psig_value value_desc ->
      let value_desc = { value_desc with
        pval_prim = compat_prim value_desc.pval_prim;
        pval_type = compat_core_type ~module_name value_desc.pval_type;
        pval_attributes = [];
      } in
      let psig_desc = Parsetree.Psig_value value_desc in
      { item with psig_desc }
  | Psig_module module_declaration ->
      let module_name =
        Longident.Ldot (module_name, module_declaration.pmd_name.txt) in
      let pmd_type =
        module_declaration.pmd_type |>
        compat_module_type ~module_name ~reference_interpreter in
      { item with psig_desc = Psig_module { module_declaration with pmd_type }}
  | Psig_modtype module_type_declaration ->
      let pmtd_type =
        module_type_declaration.pmtd_type |> Option.map @@
        compat_module_type ~module_name ~reference_interpreter in
      { item with psig_desc =
        Psig_modtype { module_type_declaration with pmtd_type }}
  | _ ->
      item

and compat_module_type ~module_name ~reference_interpreter
    (module_type : Parsetree.module_type) =
  match module_type.pmty_desc with
  | Pmty_ident
      ({ txt = Ldot (Lident "Hashtbl", "SeededHashedType") } as longident) ->
        let longident =
          { longident with txt =
            if module_name = Ldot (Lident "Hashtbl", "MakeSeeded") then
              Longident.Lident "SeededHashedType"
            else
              Longident.Ldot
                (Lident "Stdcompat__hashtbl", "SeededHashedType") } in
        { module_type with pmty_desc = Pmty_ident longident }
  | Pmty_signature signature ->
      let signature =
        List.map
          (compat_signature_item ~module_name ~reference_interpreter)
          signature in
      { module_type with pmty_desc = Pmty_signature signature }
  | Pmty_functor (var, arg, body) ->
      let arg =
        Option.map (compat_module_type ~module_name ~reference_interpreter)
          arg in
      let module_name =
        Longident.Lapply (module_name, Lident var.txt) in
      let body = compat_module_type ~module_name ~reference_interpreter body in
      { module_type with pmty_desc = Pmty_functor (var, arg, body) }
  | Pmty_alias ident ->
      module_type_of_longident ~interpreter:reference_interpreter ident.txt |>
      compat_module_type ~module_name ~reference_interpreter
  | _ -> module_type

(*
      Pprintast.signature formatter [item]
*)

let rec make_version_range is_equal versions =
  match versions with
  | [] -> []
  | (interpreter, item) :: tail ->
      match make_version_range is_equal tail with
      | (interpreter', item') :: tail
        when is_equal ~interpreter item ~interpreter' item' ->
          (interpreter', item') :: tail
      | tail ->
          (interpreter, item) :: tail

let is_prim_isomorphic kind p p' =
  match kind with
  | Ignore_attributes -> true
  | Attributes_equal -> List.equal String.equal p p'

let is_value_description_isomorphic kind
    (value_desc : Parsetree.value_description)
    (value_desc' : Parsetree.value_description) =
  Name.equal value_desc.pval_name value_desc'.pval_name &&
  is_core_type_isomorphic kind value_desc.pval_type value_desc'.pval_type &&
  is_prim_isomorphic kind value_desc.pval_prim value_desc'.pval_prim &&
  is_attributes_isomorphic kind value_desc.pval_attributes
    value_desc'.pval_attributes

let signature_of_module_type_desc interpreter
    (module_type_desc : Parsetree.module_type_desc) =
  match module_type_desc with
  | Pmty_signature s -> s
  | Pmty_alias ident ->
      signature_of_module_type (module_type_of_longident ~interpreter ident.txt)
  | _ -> failwith "signature_of_module_type_desc"

let rec is_module_type_desc_isomorphic kind
    ~interpreter (module_type_desc : Parsetree.module_type_desc)
    ~interpreter' (module_type_desc' : Parsetree.module_type_desc) =
  match module_type_desc, module_type_desc' with
  | Pmty_ident ident, Pmty_ident ident' ->
      ident.txt = ident'.txt
  | (Pmty_signature _, Pmty_alias _
  | Pmty_alias _, Pmty_signature _) when kind = Attributes_equal ->
      false
  | (Pmty_signature _ | Pmty_alias _),
    (Pmty_signature _ | Pmty_alias _) ->
      let signature =
        signature_of_module_type_desc interpreter module_type_desc in
      let signature' =
        signature_of_module_type_desc interpreter'
          module_type_desc' in
      List.equal
        (is_signature_item_isomorphic kind ~interpreter
           ~interpreter')
        signature signature'
  | Pmty_functor (x, arg, body), Pmty_functor (x', arg', body') ->
      Name.equal x x' &&
      Option.equal
        (is_module_type_isomorphic kind ~interpreter ~interpreter')
        arg arg' &&
      is_module_type_isomorphic kind ~interpreter body ~interpreter' body'
  | Pmty_with _, Pmty_with _ -> true
  | Pmty_typeof _, Pmty_typeof _ -> true
  | _ -> false

and is_module_type_isomorphic kind
    ~interpreter (module_type : Parsetree.module_type)
    ~interpreter' (module_type' : Parsetree.module_type) =
  is_module_type_desc_isomorphic kind ~interpreter module_type.pmty_desc
    ~interpreter' module_type'.pmty_desc &&
  is_attributes_isomorphic kind module_type.pmty_attributes
    module_type'.pmty_attributes

and is_module_declaration_isomorphic kind
    ~interpreter (module_declaration : Parsetree.module_declaration)
    ~interpreter' (module_declaration' : Parsetree.module_declaration) =
  Name.equal module_declaration.pmd_name module_declaration'.pmd_name &&
  is_module_type_isomorphic kind ~interpreter module_declaration.pmd_type
    ~interpreter' module_declaration'.pmd_type &&
  is_attributes_isomorphic kind module_declaration.pmd_attributes
    module_declaration'.pmd_attributes

and is_module_type_declaration_isomorphic kind
    ~interpreter (module_type_declaration : Parsetree.module_type_declaration)
    ~interpreter'
    (module_type_declaration' : Parsetree.module_type_declaration) =
  Name.equal module_type_declaration.pmtd_name
    module_type_declaration'.pmtd_name &&
  Option.equal (is_module_type_isomorphic kind ~interpreter ~interpreter')
    module_type_declaration.pmtd_type
    module_type_declaration'.pmtd_type &&
  is_attributes_isomorphic kind module_type_declaration.pmtd_attributes
    module_type_declaration'.pmtd_attributes

and is_signature_item_isomorphic kind
    ~interpreter (item : Parsetree.signature_item)
    ~interpreter' (item' : Parsetree.signature_item) =
  match item.psig_desc, item'.psig_desc with
  | Psig_type (rec_flag, type_decl), Psig_type (rec_flag', type_decl') ->
      let block = { Type_declaration_block.rec_flag; type_decl } in
      let block' =
        { Type_declaration_block.rec_flag = rec_flag';
          type_decl = type_decl' } in
      Type_declaration_block.is_isomorphic kind block block'
  | Psig_value value_desc, Psig_value value_desc' ->
      is_value_description_isomorphic kind value_desc value_desc'
  | Psig_module module_declaration, Psig_module module_declaration' ->
      is_module_declaration_isomorphic kind
        ~interpreter module_declaration
        ~interpreter' module_declaration'
  | Psig_modtype module_type_declaration,
      Psig_modtype module_type_declaration' ->
      is_module_type_declaration_isomorphic kind ~interpreter
          module_type_declaration
        ~interpreter' module_type_declaration'
  | _ ->
      failwith "is_signature_item_isomorphic"

let version_signature_item ~interpreter ~module_name ~signatures
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, type_decl) ->
      let block = { Type_declaration_block.rec_flag; type_decl } in
      let first_type_name = Type_declaration_block.get_first_type_name block in
      signatures |>
        List.filter_map (fun (interpreter, (s : Signature.t)) ->
          match String.Map.find_opt first_type_name s.types with
          | None -> None
          | Some block' ->
              if Type_declaration_block.is_isomorphic
                   Ignore_attributes block block' then
                Some (interpreter, block')
              else
                None) |>
        make_version_range
          (fun ~interpreter a ~interpreter' b ->
            Type_declaration_block.is_isomorphic Attributes_equal a b) |>
        List.map (fun (interpreter,
            (block : Type_declaration_block.t)) ->
          let type_decl =
            block.type_decl |> List.map (qualify_type_decl ~module_name) in
          let item = { item with psig_desc =
            Psig_type (block.rec_flag, type_decl) } in
          (interpreter, item)) |>
        Option.some
  | Psig_value value_desc ->
      signatures |>
        List.filter_map (fun (interpreter, (s : Signature.t)) ->
          match
            match String.Map.find_opt value_desc.pval_name.txt s.values with
            | None ->
                begin
                  match value_desc.pval_prim with
                  | [] -> None
                  | prim :: _ ->
                      let externals = get_pervasives_externals interpreter in
                      match StringHashtbl.find_opt externals prim with
                      | None -> None
                      | Some value_desc' ->
                          Some
                            { value_desc' with
                              pval_name = value_desc.pval_name }
                end
            | Some value_desc' -> Some value_desc'
          with
          | None -> None
          | Some value_desc' ->
              if is_value_description_isomorphic Ignore_attributes value_desc
                  value_desc' then
                Some (interpreter, value_desc')
              else
                None) |>
        make_version_range
          (fun ~interpreter a ~interpreter' b ->
            is_value_description_isomorphic Attributes_equal a b) |>
        List.map (fun (interpreter, value_desc) ->
          let item = { item with psig_desc = Psig_value value_desc } in
          (interpreter, item)) |>
        Option.some
  | Psig_module module_declaration ->
      signatures |>
        List.filter_map (fun (interpreter', (s : Signature.t)) ->
          match
            String.Map.find_opt module_declaration.pmd_name.txt s.modules with
          | None -> None
          | Some module_declaration' ->
              if is_module_declaration_isomorphic Ignore_attributes
                  ~interpreter module_declaration
                  ~interpreter' module_declaration' then
                let module_declaration' =
                  if
                    Version.compare interpreter'.version
                      { major = 4; minor = 2; patch = 0 } >= 0
                  then
                    { module_declaration with pmd_type = {
                      pmty_desc =
                        Pmty_alias
                          { loc = Location.none; txt =
                            Ldot
                              (module_name, module_declaration.pmd_name.txt) };
                      pmty_loc = Location.none;
                      pmty_attributes = [] }}
                  else
                    module_declaration' in
                Some (interpreter', module_declaration')
              else
                None) |>
        make_version_range
          (is_module_declaration_isomorphic Attributes_equal) |>
        List.map (fun (interpreter, module_declaration) ->
          let item = { item with psig_desc = Psig_module module_declaration } in
          (interpreter, item)) |>
        Option.some
  | Psig_modtype module_type_declaration ->
      signatures |>
        List.filter_map (fun (interpreter', (s : Signature.t)) ->
          match
            String.Map.find_opt
              module_type_declaration.pmtd_name.txt s.module_types with
          | None -> None
          | Some module_type_declaration' ->
              if is_module_type_declaration_isomorphic Ignore_attributes
                  ~interpreter module_type_declaration
                  ~interpreter' module_type_declaration' then
                Some (interpreter, module_type_declaration')
              else
                None) |>
        make_version_range
          (is_module_type_declaration_isomorphic Attributes_equal) |>
        List.map (fun (interpreter, module_type_declaration) ->
          let item = { item with
            psig_desc = Psig_modtype module_type_declaration } in
          (interpreter, item)) |>
        Option.some
  | _ ->
      None

let compare_versioned_signature versions versions' =
  try
    let interpreter, (item : Parsetree.signature_item) =
      List.last versions in
    let interpreter', (item' : Parsetree.signature_item) =
      List.last versions' in
    match item.psig_desc, item'.psig_desc with
    | Psig_value _, Psig_value _ -> - Version.compare interpreter.version interpreter'.version
    | _, Psig_value _ -> -1
    | Psig_value _, _ -> 1
    | _ -> 0
  with _ ->
    prerr_endline "EMPTY!";
    0

let type_of_desc ptyp_desc : Parsetree.core_type =
  { ptyp_desc; ptyp_loc = Location.none; ptyp_attributes = [] }

let format_block block sub formatter item =
  Format.fprintf formatter "\
    @@BEGIN_%s@@@.\
    %a@.\
    @@END_%s@@@.\
  " block sub item block

let format_block_prefix prefix block sub formatter item =
  format_block (prefix ^ "_" ^ block) sub formatter item

let format_with block = format_block_prefix "WITH" block

let format_without block = format_block_prefix "WITHOUT" block

let format_ver prefix ver =
  format_block_prefix prefix (Version.to_string ~sep:"_" ver)

let format_before ver = format_ver "BEFORE" ver

let format_from ver = format_ver "FROM" ver

let format_without block sub formatter item =
  format_block ("WITHOUT_" ^ block) sub formatter item

let format_with_without block sub formatter item_with item_without =
  format_with block sub formatter item_with;
  format_without block sub formatter item_without

let format_default_item ~module_name formatter
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, [{ ptype_name = { txt = "result" }} as type_decl]) ->
      let ptyp_desc = Parsetree.Ptyp_constr (
        { txt = Ldot (Lident "Result", "result"); loc = Location.none },
        [type_of_desc (Ptyp_var "a"); type_of_desc (Ptyp_var "b")]) in
      let manifest = type_of_desc ptyp_desc in
      let type_decl = { type_decl with ptype_manifest = Some manifest } in
      let psig_desc = Parsetree.Psig_type (rec_flag, [type_decl]) in
      let result_item = { item with psig_desc } in
      format_with_without "RESULT_PKG"
        Pprintast.signature formatter [result_item] [item]
  | Psig_type (rec_flag, [{ ptype_name = { txt = "t" }} as type_decl])
      when module_name = Longident.Lident "Uchar" ->
      let ptyp_desc = Parsetree.Ptyp_constr (
        { txt = Ldot (Lident "Uchar", "t"); loc = Location.none },
        []) in
      let manifest = type_of_desc ptyp_desc in
      let type_decl = { type_decl with ptype_manifest = Some manifest } in
      let psig_desc = Parsetree.Psig_type (rec_flag, [type_decl]) in
      let result_item = { item with psig_desc } in
      format_with_without "UCHAR_PKG"
        Pprintast.signature formatter [result_item] [item]
  | Psig_type (_, _)
      when module_name = Longident.Lident "Seq" ->
      Format.fprintf formatter "\
type 'a t = unit -> 'a node
and 'a node =
%a
  | Nil
  | Cons of 'a * 'a t"
        (format_with "SEQ_PKG" Format.pp_print_string)
         "  'a Seq.node ="
  | _ ->
      Format.fprintf formatter "%a" Pprintast.signature [item]

let item_name module_name (item : Parsetree.signature_item) =
  let name =
    match item.psig_desc with
    | Psig_type (rec_flag, type_decl :: _) ->
        type_decl.ptype_name.txt
    | Psig_value value_desc ->
        value_desc.pval_name.txt
    | Psig_module module_declaration ->
        module_declaration.pmd_name.txt
    | Psig_modtype module_type_declaration ->
        module_type_declaration.pmtd_name.txt
    | _ -> assert false in
  Printf.sprintf "%s.%s" (string_of_longident module_name) name

let add_self_type_manifest_to_type_decl ~module_name
    (type_decl : Parsetree.type_declaration) =
  match type_decl.ptype_manifest with
  | None ->
      { type_decl with ptype_manifest =
        let params = type_decl.ptype_params |> List.map fst in
        Some ({ ptyp_desc =
          Ptyp_constr ({ loc = Location.none; txt =
            Ldot (module_name, type_decl.ptype_name.txt) }, params);
              ptyp_loc = Location.none; ptyp_attributes = [] })}
  | Some manifest -> type_decl

let rec add_self_type_manifest ~module_name (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, type_decl_list) ->
      let type_decl_list = type_decl_list |>
        List.map (add_self_type_manifest_to_type_decl ~module_name) in
      { item with psig_desc = Psig_type (rec_flag, type_decl_list) }
  | Psig_value _ | Psig_modtype _ -> item
  | Psig_module module_declaration ->
      { item with psig_desc = Psig_module { module_declaration with
        pmd_type = module_declaration.pmd_type |>
          (add_self_type_manifest_to_module_type ~module_name) }}
(*  | Psig_modtype module_type_declaration ->
      { item with psig_desc = Psig_modtype { module_type_declaration with
        pmtd_type = module_type_declaration.pmtd_type |>
          Option.map (add_self_type_manifest_to_module_type ~module_name) }} *)
  | _ -> assert false

and add_self_type_manifest_to_module_type ~module_name
    (module_type : Parsetree.module_type) =
  match module_type.pmty_desc with
  | Pmty_signature signature ->
      let signature = signature |>
        List.map (add_self_type_manifest ~module_name) in
      { module_type with pmty_desc = Pmty_signature signature }
  | Pmty_functor (var, arg, body) ->
(*
      let arg = arg |> Option.map
        (add_self_type_manifest_to_module_type ~module_name) in
*)
      let body = body |> add_self_type_manifest_to_module_type ~module_name in
      { module_type with pmty_desc = Pmty_functor (var, arg, body) }
  | Pmty_with (ty, cstr) ->
      { module_type with pmty_desc =
        Pmty_with
          (add_self_type_manifest_to_module_type ~module_name ty, cstr) }
  | _ -> module_type


let format_versioned_signature ~module_name ~version_high ~version_low
    ~reference_interpreter formatter versions =
  match versions with
  | [last_interpreter, item] when last_interpreter.version = version_low ->
      let self_item = add_self_type_manifest ~module_name item in
      Format.fprintf formatter "%a@." Pprintast.signature [self_item];
      let item_name = item_name module_name item in
      Format.fprintf formatter "(** Alias for {!%s} *)@." item_name
  | (last_interpreter, item) :: tail ->
      let self_item = add_self_type_manifest ~module_name item in
      let default_item =
        compat_signature_item ~module_name ~reference_interpreter item in
      begin
        if tail = [] &&
          is_signature_item_isomorphic Attributes_equal
            ~interpreter:last_interpreter self_item
            ~interpreter':last_interpreter default_item then
          Format.fprintf formatter "%a@." Pprintast.signature [self_item]
        else
          begin
            format_from last_interpreter.version Pprintast.signature
              formatter [self_item];
            let rec format_tail version tail =
              let format formatter tail =
                match tail with
                | [] ->
                    format_default_item ~module_name formatter default_item
                | [last_interpreter, item] ->
                    let self_item = add_self_type_manifest ~module_name item in
                    if last_interpreter.version = version_low
                  || is_signature_item_isomorphic Attributes_equal
                      ~interpreter:last_interpreter
                      self_item ~interpreter':last_interpreter default_item then
                      Pprintast.signature formatter [self_item]
                  else
                    begin
                      format_from last_interpreter.version
                        Pprintast.signature formatter [self_item];
                      format_before last_interpreter.version
                        Pprintast.signature formatter [default_item]
                    end
              | (last_interpreter, item) :: tail ->
                  let self_item = add_self_type_manifest ~module_name item in
                  format_from last_interpreter.version
                    Pprintast.signature formatter [self_item];
                  format_tail last_interpreter.version tail in
              format_before version format formatter tail in
            format_tail last_interpreter.version tail
          end
      end;
      let format_doc formatter versions =
        let format_doc_version (interpreter, item) =
          Format.fprintf formatter "@[@since %s:@ %a@]@."
            (Version.to_string interpreter.version)
            Pprintast.signature [item] in
        List.iter format_doc_version versions in
      Format.fprintf formatter "(** @[<v>%a@] *)@." format_doc versions
  | [] ->
      prerr_endline "EMPTY!"

let main argv =
  let arg_list = Array.to_list Sys.argv in
  let module_name, command_lines =
    match arg_list with
    | _ :: module_name :: command_lines ->
        module_name, command_lines
    | [] | [_] -> failwith "No argument given" in
  let signatures =
    command_lines |> List.map @@ fun command_line ->
      let version = Version.of_command_line command_line in
      let interpreter = { command_line; version } in
      let signature = module_type_of_name ~interpreter ~module_name |>
        signature_of_module_type in
      interpreter, signature in
  let reference_interpreter, reference_signature =
    match signatures with
    | first :: _ -> first
    | _ -> failwith "No reference version" in
  let signatures = signatures |> List.map @@ fun (interpreter, s) ->
    interpreter, Signature.of_parsetree s in
  let module_name = Longident.Lident module_name in
  let versioned_signature =
    reference_signature |> List.filter_map @@
      version_signature_item ~interpreter:reference_interpreter
        ~module_name ~signatures in
  let { version = version_high }, _ = List.hd signatures in
  let { version = version_low }, _ = List.last signatures in
  versioned_signature |>
    List.sort compare_versioned_signature |>
    List.iter @@
      format_versioned_signature ~module_name
      ~version_high ~version_low
      ~reference_interpreter
      Format.std_formatter
(*
  let mli_filenames = argv |> Array.to_list |> List.tl in
  let signatures = mli_filenames |> List.map read_interface in
  let main_signature, other_signatures =
    match signatures with
    | [] -> failwith "No mli file given"
    | main_signature :: other_signatures ->
        main_signature, other_signatures in
  main_signature |> List.iter @@ format_signature_item Format.std_formatter
*)

let () =
  if not !Sys.interactive then
    Sys.argv |> main
