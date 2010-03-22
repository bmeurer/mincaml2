type description =
    { prim_name:         string;
      prim_arity:        int;
      prim_alloc:        bool;
      prim_native_name:  string; 
      prim_native_float: bool }

val parse_declaration: int -> string list -> description
