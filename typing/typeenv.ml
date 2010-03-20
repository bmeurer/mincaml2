open Types

module IdentMap = Rbmap.Make(Ident)
module StringMap = Rbmap.Make(String)

type t =
    { types: type_declaration IdentMap.t;
      types_mapping: Ident.t StringMap.t }

let empty =
  { types = IdentMap.empty;
    types_mapping = StringMap.empty }

let lookup_type id gamma =
  IdentMap.find id gamma.types

let find_type name gamma = 
  let id = StringMap.find name gamma.types_mapping in
    id, lookup_type id gamma

let add_type id decl gamma =
  let name = Ident.name id in
    { gamma with
        types = IdentMap.add id decl gamma.types;
        types_mapping = StringMap.add name id gamma.types_mapping }
      
