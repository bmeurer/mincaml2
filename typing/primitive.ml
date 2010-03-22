type description =
    { prim_name:         string;
      prim_arity:        int;
      prim_alloc:        bool;
      prim_native_name:  string; 
      prim_native_float: bool }

let parse_declaration arity pdecl =
  match pdecl with
    | [name; "noalloc"; native_name; "float"] ->
        { prim_name = name;
          prim_arity = arity;
          prim_alloc = false;
          prim_native_name = native_name;
          prim_native_float = true }
    | [name; "noalloc"; native_name] ->
        { prim_name = name;
          prim_arity = arity;
          prim_alloc = false;
          prim_native_name = native_name;
          prim_native_float = false }
    | [name; native_name; "float"] ->
        { prim_name = name;
          prim_arity = arity;
          prim_alloc = true;
          prim_native_name = native_name;
          prim_native_float = true }
    | [name; "noalloc"] ->
        { prim_name = name;
          prim_arity = arity;
          prim_alloc = false;
          prim_native_name = "";
          prim_native_float = false }
    | [name; native_name] ->
        { prim_name = name;
          prim_arity = arity;
          prim_alloc = true;
          prim_native_name = native_name;
          prim_native_float = false }
    | [name] ->
        { prim_name = name;
          prim_arity = arity;
          prim_alloc = true;
          prim_native_name = "";
          prim_native_float = false }
    | _ ->
        invalid_arg "Primitive.parse_declaration"
