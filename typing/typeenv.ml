open Types

module IdentMap = Rbmap.Make(Ident)
module StringMap = Rbmap.Make(String)

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
