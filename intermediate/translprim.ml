open Lambda
open Primitive
open Typedast
open Typeopt
open Types

type primitive_comparison =
    { pcmp_int:       primitive;
      pcmp_float:     primitive;
      pcmp_string:    primitive;
      pcmp_nativeint: primitive;
      pcmp_int32:     primitive;
      pcmp_int64:     primitive;
      pcmp_generic:   primitive;
      pcmp_optccstr:  bool }     (* Optimize comparison with constant constructor? *)

let primitive_comparisons = HashtblUtils.create 11
  [
    "%equal",
    { pcmp_int = Pintcmp(Ceq);
      pcmp_float = Pfloatcmp(Ceq);
      pcmp_string = Pextcall({ prim_name = "mincaml2_string_equal";
                               prim_arity = 2;
                               prim_alloc = false;
                               prim_native_name = "";
                               prim_native_float = false });
      pcmp_nativeint = Pbintcmp(Pnativeint, Ceq);
      pcmp_int32 = Pbintcmp(Pint32, Ceq);
      pcmp_int64 = Pbintcmp(Pint64, Ceq);
      pcmp_generic = Pextcall({ prim_name = "mincaml2_equal";
                                prim_arity = 2;
                                prim_alloc = true;
                                prim_native_name = "";
                                prim_native_float = false });
      pcmp_optccstr = true };
    "%notequal",
    { pcmp_int = Pintcmp(Cne);
      pcmp_float = Pfloatcmp(Cne);
      pcmp_string = Pextcall({ prim_name = "mincaml2_string_notequal";
                               prim_arity = 2;
                               prim_alloc = false;
                               prim_native_name = "";
                               prim_native_float = false });
      pcmp_nativeint = Pbintcmp(Pnativeint, Cne);
      pcmp_int32 = Pbintcmp(Pint32, Cne);
      pcmp_int64 = Pbintcmp(Pint64, Cne);
      pcmp_generic = Pextcall({ prim_name = "mincaml2_notequal";
                                prim_arity = 2;
                                prim_alloc = true;
                                prim_native_name = "";
                                prim_native_float = false });
      pcmp_optccstr = true };
    "%lessthan",
    { pcmp_int = Pintcmp(Clt);
      pcmp_float = Pfloatcmp(Clt);
      pcmp_string = Pextcall({ prim_name = "mincaml2_string_lessthan";
                               prim_arity = 2;
                               prim_alloc = false;
                               prim_native_name = "";
                               prim_native_float = false });
      pcmp_nativeint = Pbintcmp(Pnativeint, Clt);
      pcmp_int32 = Pbintcmp(Pint32, Clt);
      pcmp_int64 = Pbintcmp(Pint64, Clt);
      pcmp_generic = Pextcall({ prim_name = "mincaml2_lessthan";
                                prim_arity = 2;
                                prim_alloc = true;
                                prim_native_name = "";
                                prim_native_float = false });
      pcmp_optccstr = false };
    "%greaterthan",
    { pcmp_int = Pintcmp(Cgt);
      pcmp_float = Pfloatcmp(Cgt);
      pcmp_string = Pextcall({ prim_name = "mincaml2_string_greaterthan";
                               prim_arity = 2;
                               prim_alloc = false;
                               prim_native_name = "";
                               prim_native_float = false });
      pcmp_nativeint = Pbintcmp(Pnativeint, Cgt);
      pcmp_int32 = Pbintcmp(Pint32, Cgt);
      pcmp_int64 = Pbintcmp(Pint64, Cgt);
      pcmp_generic = Pextcall({ prim_name = "mincaml2_greaterthan";
                                prim_arity = 2;
                                prim_alloc = true;
                                prim_native_name = "";
                                prim_native_float = false });
      pcmp_optccstr = false };
    "%lessequal",
    { pcmp_int = Pintcmp(Cle);
      pcmp_float = Pfloatcmp(Cle);
      pcmp_string = Pextcall({ prim_name = "mincaml2_string_lessequal";
                               prim_arity = 2;
                               prim_alloc = false;
                               prim_native_name = "";
                               prim_native_float = false });
      pcmp_nativeint = Pbintcmp(Pnativeint, Cle);
      pcmp_int32 = Pbintcmp(Pint32, Cle);
      pcmp_int64 = Pbintcmp(Pint64, Cle);
      pcmp_generic = Pextcall({ prim_name = "mincaml2_lessequal";
                                prim_arity = 2;
                                prim_alloc = true;
                                prim_native_name = "";
                                prim_native_float = false });
      pcmp_optccstr = false };
    "%greaterequal",
    { pcmp_int = Pintcmp(Cge);
      pcmp_float = Pfloatcmp(Cge);
      pcmp_string = Pextcall({ prim_name = "mincaml2_string_greaterequal";
                               prim_arity = 2;
                               prim_alloc = false;
                               prim_native_name = "";
                               prim_native_float = false });
      pcmp_nativeint = Pbintcmp(Pnativeint, Cge);
      pcmp_int32 = Pbintcmp(Pint32, Cge);
      pcmp_int64 = Pbintcmp(Pint64, Cge);
      pcmp_generic = Pextcall({ prim_name = "mincaml2_greaterequal";
                                prim_arity = 2;
                                prim_alloc = true;
                                prim_native_name = "";
                                prim_native_float = false });
      pcmp_optccstr = false };
    "%compare",
    { pcmp_int = Psubint;
      pcmp_float = Pextcall({ prim_name = "mincaml2_float_compare";
                              prim_arity = 2;
                              prim_alloc = false;
                              prim_native_name = "";
                              prim_native_float = false });
      pcmp_string = Pextcall({ prim_name = "mincaml2_string_compare";
                               prim_arity = 2;
                               prim_alloc = false;
                               prim_native_name = "";
                               prim_native_float = false });
      pcmp_nativeint = Pextcall({ prim_name = "mincaml2_nativeint_compare";
                                  prim_arity = 2;
                                  prim_alloc = false;
                                  prim_native_name = "";
                                  prim_native_float = false });
      pcmp_int32 = Pextcall({ prim_name = "mincaml2_int32_compare";
                              prim_arity = 2;
                              prim_alloc = false;
                              prim_native_name = "";
                              prim_native_float = false });
      pcmp_int64 = Pextcall({ prim_name = "mincaml2_int64_compare";
                              prim_arity = 2;
                              prim_alloc = false;
                              prim_native_name = "";
                              prim_native_float = false });
      pcmp_generic = Pextcall({ prim_name = "mincaml2_compare";
                                prim_arity = 2;
                                prim_alloc = true;
                                prim_native_name = "";
                                prim_native_float = false });
      pcmp_optccstr = false }
  ]

let primitives = HashtblUtils.create 33
  [
    "%identity", Pidentity;
    "%ignore", Pignore;
  ]

let translate_primitive prim expl =
  try
    let pcmp = Hashtbl.find primitive_comparisons prim.prim_name in
      assert (List.length expl = 2);
      begin match expl with
        | { exp_desc = Texp_construct({ cstr_blocks = 0 }, _) } :: _
        | _ :: { exp_desc = Texp_construct({ cstr_blocks = 0 }, _) } :: _ ->
            pcmp.pcmp_int
        | { exp_desc = Texp_construct({ cstr_tag = Cstr_constant(_) }, _) } :: _
        | _ :: { exp_desc = Texp_construct({ cstr_tag = Cstr_constant(_) }, _) } :: _ when pcmp.pcmp_optccstr ->
            pcmp.pcmp_int
        | exp :: _ when has_base_type Typeenv.ident_char exp || has_base_type Typeenv.ident_int exp ->
            pcmp.pcmp_int
        | exp :: _ when has_base_type Typeenv.ident_float exp ->
            pcmp.pcmp_float
        | exp :: _ when has_base_type Typeenv.ident_string exp ->
            pcmp.pcmp_string
        | exp :: _ when has_base_type Typeenv.ident_nativeint exp ->
            pcmp.pcmp_nativeint
        | exp :: _ when has_base_type Typeenv.ident_int32 exp ->
            pcmp.pcmp_int32
        | exp :: _ when has_base_type Typeenv.ident_int64 exp ->
            pcmp.pcmp_int64
        | _ ->
            pcmp.pcmp_generic
      end
  with
    | Not_found ->
        try
          Hashtbl.find primitives prim.prim_name
        with
          | Not_found ->
              Pextcall(prim)

let translate_primitive_standalone prim =
  let p = (try
             let pcmp = Hashtbl.find primitive_comparisons prim.prim_name in
               pcmp.pcmp_generic
           with
             | Not_found ->
                 try
                   Hashtbl.find primitives prim.prim_name
                 with
                   | Not_found ->
                       Pextcall(prim)) in
  let idl = ListUtils.init prim.prim_arity Ident.create_tmp in
    Lfunction(idl, Lprim(p, List.map (fun id -> Lident(id)) idl))
