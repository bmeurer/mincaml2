open Types

module IdentMap = Rbmap.Make(Ident)
module StringMap = Rbmap.Make(String)

(* Qualified identifiers for predefined types *)
let ident_int = Ident.create "int"
and ident_char = Ident.create "char"
and ident_float = Ident.create "float"
and ident_int32 = Ident.create "int32"
and ident_int64 = Ident.create "int64"
and ident_string = Ident.create "string"
and ident_nativeint = Ident.create "nativeint"
and ident_exn = Ident.create "exn"
and ident_unit = Ident.create "unit"
and ident_bool = Ident.create "bool"
and ident_list = Ident.create "list"
and ident_option = Ident.create "option"

(* Qualified identifiers for predefined exceptions *)
let ident_match_failure = Ident.create "Match_failure"
and ident_out_of_memory = Ident.create "Out_of_memory"
and ident_stack_overflow = Ident.create "Stack_overflow"
and ident_invalid_argument = Ident.create "Invalid_argument"
and ident_failure = Ident.create "Failure"
and ident_not_found = Ident.create "Not_found"
and ident_division_by_zero = Ident.create "Division_by_zero"

(* Predefined types *)
let type_int = new_generic_typ (Tconstruct(ident_int, []))
and type_char = new_generic_typ (Tconstruct(ident_char, []))
and type_float = new_generic_typ (Tconstruct(ident_float, []))
and type_int32 = new_generic_typ (Tconstruct(ident_int32, []))
and type_int64 = new_generic_typ (Tconstruct(ident_int64, []))
and type_string = new_generic_typ (Tconstruct(ident_string, []))
and type_nativeint = new_generic_typ (Tconstruct(ident_nativeint, []))
and type_exn = new_generic_typ (Tconstruct(ident_exn, []))
and type_unit = new_generic_typ (Tconstruct(ident_unit, []))
and type_bool = new_generic_typ (Tconstruct(ident_bool, []))
and type_list tau = new_generic_typ (Tconstruct(ident_list, [tau]))
and type_option tau = new_generic_typ (Tconstruct(ident_option, [tau]))
and type_arrow tau1 tau2 = new_generic_typ (Tarrow(tau1, tau2))


(****************************************************)
(*** Determine constructors for type declarations ***)
(****************************************************)

let constructors_of_variants cstr_type variants =
  let rec constructors_of_variants_aux cstr_tag = function
    | [] ->
        []
    | (cstr_id, cstr_args) :: variants ->
        let cstr = { cstr_type = cstr_type;
                     cstr_args = cstr_args;
                     cstr_arity = List.length cstr_args;
                     cstr_tag = cstr_tag } in
          (cstr_id, cstr) :: constructors_of_variants_aux (cstr_tag + 1) variants
  in constructors_of_variants_aux 0 variants

let constructors_of_decl id decl =
  match decl.type_desc with
    | Type_abstract
    | Type_abbrev(_) -> []
    | Type_variant(variants) -> constructors_of_variants (new_generic_typ (Tconstruct(id, decl.type_params))) variants


(*********************************)
(*** Type environment handling ***)
(*********************************)

type t =
    { cstrs:          constructor_description IdentMap.t;
      cstrs_mapping:  Ident.t StringMap.t;
      types:          type_declaration IdentMap.t;
      types_mapping:  Ident.t StringMap.t;
      values:         value_description IdentMap.t;
      values_mapping: Ident.t StringMap.t }

let empty =
  { cstrs = IdentMap.empty;
    cstrs_mapping = StringMap.empty;
    types = IdentMap.empty;
    types_mapping = StringMap.empty;
    values = IdentMap.empty; 
    values_mapping = StringMap.empty }

let lookup_cstr id gamma =
  IdentMap.find id gamma.cstrs

let lookup_type id gamma =
  IdentMap.find id gamma.types

let lookup_value id gamma =
  IdentMap.find id gamma.values

let find_cstr name gamma =
  let id = StringMap.find name gamma.cstrs_mapping in
    id, lookup_cstr id gamma

let find_type name gamma = 
  let id = StringMap.find name gamma.types_mapping in
    id, lookup_type id gamma

let find_value name gamma =
  let id = StringMap.find name gamma.values_mapping in
    id, lookup_value id gamma

let add_cstr id cstr gamma =
  let name = Ident.name id in
    { gamma with
        cstrs = IdentMap.add id cstr gamma.cstrs;
        cstrs_mapping = StringMap.add name id gamma.cstrs_mapping }

let add_exn id taul gamma =
  (* figure out the exn type decl and its variants *)
  let exn_decl = lookup_type ident_exn gamma in
  let exn_variants = (match exn_decl.type_desc with Type_variant(variants) -> variants | _ -> assert false) in
  (* create the new exception constructor *)
  let cstr = { cstr_type = instantiate type_exn;
               cstr_args = taul;
               cstr_arity = List.length taul;
               cstr_tag = List.length exn_variants } in
    (* add the new exception to the exn variant, this way each
       and every gamma will have the full exn variant *)
    exn_decl.type_desc <- Type_variant((id, taul) :: exn_variants);
    (* add the new exception constructor *)
    add_cstr id cstr gamma

let add_type id decl gamma =
  let gamma = (List.fold_left
                 (fun gamma (id, cstr) -> add_cstr id cstr gamma)
                 gamma
                 (constructors_of_decl id decl)) in
  let name = Ident.name id in
    { gamma with
        types = IdentMap.add id decl gamma.types;
        types_mapping = StringMap.add name id gamma.types_mapping }

let add_types iddecls gamma =
  List.fold_left (fun gamma (id, decl) -> add_type id decl gamma) gamma iddecls
      
let add_value id value gamma =
  let name = Ident.name id in
    { gamma with
        values = IdentMap.add id value gamma.values;
        values_mapping = StringMap.add name id gamma.values_mapping }

let initial =
  let decl_abstr =
    { type_params = [];
      type_arity = 0;
      type_desc = Type_abstract }
  and decl_exn =
    { type_params = [];
      type_arity = 0;
      type_desc = Type_variant([]) }
  and decl_bool =
    { type_params = [];
      type_arity = 0;
      type_desc = Type_variant([Ident.create "false", [];
                                Ident.create "true", []]) }
  and decl_unit =
    { type_params = [];
      type_arity = 0;
      type_desc = Type_variant([Ident.create "()", []]) }
  and decl_list =
    let tau = new_generic_var () in
      { type_params = [tau];
        type_arity = 1;
        type_desc = Type_variant([Ident.create "[]", [];
                                  Ident.create "::", [tau; type_list tau]]) }
  and decl_option =
    let tau = new_generic_var () in
      { type_params = [tau];
        type_arity = 1;
        type_desc = Type_variant([Ident.create "None", [];
                                  Ident.create "Some", [tau]]) } in
  let gamma = empty in
  let gamma = (List.fold_left
                 (fun gamma (id, decl) -> add_type id decl gamma)
                 gamma
                 [
                   ident_int, decl_abstr;
                   ident_char, decl_abstr;
                   ident_float, decl_abstr;
                   ident_int32, decl_abstr;
                   ident_int64, decl_abstr;
                   ident_string, decl_abstr;
                   ident_nativeint, decl_abstr;
                   ident_exn, decl_exn;
                   ident_unit, decl_unit;
                   ident_bool, decl_bool;
                   ident_list, decl_list;
                   ident_option, decl_option
                 ]) in
  let gamma = (List.fold_left
                 (fun gamma (id, taul) -> add_exn id taul gamma)
                 gamma
                 [
                   ident_match_failure, [new_generic_typ (Ttuple([type_string; type_int; type_int]))];
                   ident_out_of_memory, [];
                   ident_stack_overflow, [];
                   ident_invalid_argument, [];
                   ident_failure, [type_string];
                   ident_not_found, [];
                   ident_division_by_zero, []
                 ]) in
  let gamma = (List.fold_left
                 (fun gamma (name, tau) ->
                    add_value (Ident.create name) { val_kind = Val_regular; val_tau = tau } gamma)
                 gamma
                 [
                   ("=", let tau = new_generic_var () in type_arrow tau (type_arrow tau type_bool));
                   ("<>", let tau = new_generic_var () in type_arrow tau (type_arrow tau type_bool));
                   ("<=", let tau = new_generic_var () in type_arrow tau (type_arrow tau type_bool));
                   ("<", let tau = new_generic_var () in type_arrow tau (type_arrow tau type_bool));
                   (">", let tau = new_generic_var () in type_arrow tau (type_arrow tau type_bool));
                   (">=", let tau = new_generic_var () in type_arrow tau (type_arrow tau type_bool));
                   ("==", let tau = new_generic_var () in type_arrow tau (type_arrow tau type_bool));
                   ("!=", let tau = new_generic_var () in type_arrow tau (type_arrow tau type_bool));
                   ("+", type_arrow type_int (type_arrow type_int type_int));
                   ("-", type_arrow type_int (type_arrow type_int type_int));
                   ("*", type_arrow type_int (type_arrow type_int type_int));
                   ("+.", type_arrow type_float (type_arrow type_float type_float));
                   ("-.", type_arrow type_float (type_arrow type_float type_float));
                   ("*.", type_arrow type_float (type_arrow type_float type_float));
                 ]) in
    gamma
