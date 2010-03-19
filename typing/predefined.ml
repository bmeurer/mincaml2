open Types

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

