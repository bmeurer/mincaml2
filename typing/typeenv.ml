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
let type_int = Tconstruct(ident_int, [])
let type_char = Tconstruct(ident_char, [])
let type_float = Tconstruct(ident_float, [])
let type_int32 = Tconstruct(ident_int32, [])
let type_int64 = Tconstruct(ident_int64, [])
let type_string = Tconstruct(ident_string, [])
let type_nativeint = Tconstruct(ident_nativeint, [])
let type_exn = Tconstruct(ident_exn, [])
let type_unit = Tconstruct(ident_unit, [])
let type_bool = Tconstruct(ident_bool, [])
let type_list tau = Tconstruct(ident_list, [tau])
let type_option tau = Tconstruct(ident_option, [tau])

type t =
    { types:          type_declaration IdentMap.t;
      types_mapping:  Ident.t StringMap.t;
      values:         value_description IdentMap.t;
      values_mapping: Ident.t StringMap.t }

let empty =
  { types = IdentMap.empty;
    types_mapping = StringMap.empty;
    values = IdentMap.empty; 
    values_mapping = StringMap.empty }

let lookup_type id gamma =
  IdentMap.find id gamma.types

let lookup_value id gamma =
  IdentMap.find id gamma.values

let find_type name gamma = 
  let id = StringMap.find name gamma.types_mapping in
    id, lookup_type id gamma

let find_value name gamma =
  let id = StringMap.find name gamma.values_mapping in
    id, lookup_value id gamma

let add_type id decl gamma =
  let name = Ident.name id in
    { gamma with
        types = IdentMap.add id decl gamma.types;
        types_mapping = StringMap.add name id gamma.types_mapping }
      
let add_value id value gamma =
  let name = Ident.name id in
    { gamma with
        values = IdentMap.add id value gamma.values;
        values_mapping = StringMap.add name id gamma.values_mapping }
