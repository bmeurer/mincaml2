open Types

module IdentMap = Map.Make(Ident)
module StringMap = Map.Make(String)

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
  let cstr_consts = ref 0 and cstr_blocks = ref 0 in
    (* TODO - check block constructor limit *)
    List.iter (function (_, []) -> incr cstr_consts | _ -> incr cstr_blocks) variants;
    let rec constructors_of_variants_aux const_tag block_tag = function
      | [] ->
          []
      | (cstr_name, cstr_args) :: variants ->
          let cstr_tag, variants = (match cstr_args with
                                      | [] -> (Cstr_constant const_tag,
                                               constructors_of_variants_aux (const_tag + 1) block_tag variants)
                                      | _ -> (Cstr_block block_tag,
                                              constructors_of_variants_aux const_tag (block_tag + 1) variants)) in
          let cstr = { cstr_type = cstr_type;
                       cstr_args = cstr_args;
                       cstr_arity = List.length cstr_args;
                       cstr_tag = cstr_tag;
                       cstr_consts = !cstr_consts;
                       cstr_blocks = !cstr_blocks } in
            (cstr_name, cstr) :: variants
    in constructors_of_variants_aux 0 0 variants

let constructors_of_decl id decl =
  match decl.type_desc with
    | Type_abstract
    | Type_abbrev(_) -> []
    | Type_variant(variants) -> constructors_of_variants (new_generic_typ (Tconstruct(id, decl.type_params))) variants


(*********************************)
(*** Type environment handling ***)
(*********************************)

type t =
    { cstrs:          constructor_description StringMap.t;
      types:          type_declaration IdentMap.t;
      types_mapping:  Ident.t StringMap.t;
      values:         value_description IdentMap.t;
      values_mapping: Ident.t StringMap.t }

let empty =
  { cstrs = StringMap.empty;
    types = IdentMap.empty;
    types_mapping = StringMap.empty;
    values = IdentMap.empty; 
    values_mapping = StringMap.empty }

let lookup_type id gamma =
  IdentMap.find id gamma.types

let lookup_value id gamma =
  IdentMap.find id gamma.values

let find_cstr name gamma =
  StringMap.find name gamma.cstrs

let find_type name gamma = 
  let id = StringMap.find name gamma.types_mapping in
    id, lookup_type id gamma

let find_value name gamma =
  let id = StringMap.find name gamma.values_mapping in
    id, lookup_value id gamma

let add_cstr name cstr gamma =
    { gamma with
        cstrs = StringMap.add name cstr gamma.cstrs }

let add_exn id taul gamma =
  let cstr = { cstr_type = instantiate type_exn;
               cstr_args = taul;
               cstr_arity = List.length taul;
               cstr_tag = Cstr_exception(id);
               cstr_consts = -1;
               cstr_blocks = -1 } in
    add_cstr (Ident.name id) cstr gamma

let add_type id decl gamma =
  let gamma = (List.fold_left
                 (fun gamma (name, cstr) -> add_cstr name cstr gamma)
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
      type_desc = Type_variant(["false", []; "true", []]) }
  and decl_unit =
    { type_params = [];
      type_arity = 0;
      type_desc = Type_variant(["()", []]) }
  and decl_list =
    let tau = new_generic_var () in
      { type_params = [tau];
        type_arity = 1;
        type_desc = Type_variant(["[]", []; "::", [tau; type_list tau]]) }
  and decl_option =
    let tau = new_generic_var () in
      { type_params = [tau];
        type_arity = 1;
        type_desc = Type_variant(["None", []; "Some", [tau]]) } in
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
    gamma
