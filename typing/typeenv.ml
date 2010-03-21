open Types

module IdentMap = Rbmap.Make(Ident)
module StringMap = Rbmap.Make(String)

(* Qualified identifiers for predefined types *)
let ident_int = Ident.create "int"
let ident_char = Ident.create "char"
let ident_float = Ident.create "float"
let ident_int32 = Ident.create "int32"
let ident_int64 = Ident.create "int64"
let ident_string = Ident.create "string"
let ident_nativeint = Ident.create "nativeint"
let ident_exn = Ident.create "exn"
let ident_unit = Ident.create "unit"
let ident_bool = Ident.create "bool"
let ident_list = Ident.create "list"
let ident_option = Ident.create "option"

(* Predefined types *)
let type_int = new_generic_typ (Tconstruct(ident_int, []))
let type_char = new_generic_typ (Tconstruct(ident_char, []))
let type_float = new_generic_typ (Tconstruct(ident_float, []))
let type_int32 = new_generic_typ (Tconstruct(ident_int32, []))
let type_int64 = new_generic_typ (Tconstruct(ident_int64, []))
let type_string = new_generic_typ (Tconstruct(ident_string, []))
let type_nativeint = new_generic_typ (Tconstruct(ident_nativeint, []))
let type_exn = new_generic_typ (Tconstruct(ident_exn, []))
let type_unit = new_generic_typ (Tconstruct(ident_unit, []))
let type_bool = new_generic_typ (Tconstruct(ident_bool, []))
let type_list tau = new_generic_typ (Tconstruct(ident_list, [tau]))
let type_option tau = new_generic_typ (Tconstruct(ident_option, [tau]))


(****************************************************)
(*** Determine constructors for type declarations ***)
(****************************************************)

let constructors_of_variants cstr_type variants =
  let rec constructors_of_variants_aux cstr_tag = function
    | [] ->
        []
    | (cstr_name, cstr_args) :: variants ->
        let cstr = { cstr_type = cstr_type;
                     cstr_args = cstr_args;
                     cstr_arity = List.length cstr_args;
                     cstr_tag = cstr_tag } in
          (cstr_name, cstr) :: constructors_of_variants_aux (cstr_tag + 1) variants
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

let add_type id decl gamma =
  let gamma = (List.fold_left
                 (fun gamma (name, cstr) -> add_cstr (Ident.create name) cstr gamma)
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
