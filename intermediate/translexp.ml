open Astcommon
open Lambda
open Primitive
open Typedast
open Typeenv
open Types
open Typing


(*********************)
(*** Miscellaneous ***)
(*********************)

exception Not_constant

let extract_constant = function
  | Lconst(sc) -> sc
  | _ -> raise Not_constant


(*********************************)
(*** Translation of primitives ***)
(*********************************)

let primitive_comparisons = HashtblUtils.create 7
  [
    "%equal",        Ceq;
    "%notequal",     Cne;
    "%lessthan",     Clt;
    "%greaterthan",  Cgt;
    "%lessequal",    Cle;
    "%greaterequal", Cge;
    "%compare",      Ccmp
  ]

let primitives = HashtblUtils.create 33
  [
    "%identity", Pidentity;
    "%ignore",   Pignore;
    "%eq",       Pintcmp(Ceq);
    "%noteq",    Pintcmp(Cne);
  ]

let translate_primitive gamma prim tau = 
  try
    let cmp = Hashtbl.find primitive_comparisons prim.prim_name in
      if (instance_of gamma tau (type_arrow type_int (type_arrow type_int type_bool))
          || instance_of gamma tau (type_arrow type_char (type_arrow type_char type_bool))) then
        Pintcmp(cmp)
      else if instance_of gamma tau (type_arrow type_float (type_arrow type_float type_bool)) then
        Pfloatcmp(cmp)
      else if instance_of gamma tau (type_arrow type_int32 (type_arrow type_int32 type_bool)) then
        Pbintcmp(Pint32, cmp)
      else if instance_of gamma tau (type_arrow type_int64 (type_arrow type_int64 type_bool)) then
        Pbintcmp(Pint64, cmp)
      else if instance_of gamma tau (type_arrow type_string (type_arrow type_string type_bool)) then
        Pstringcmp(cmp)
      else if instance_of gamma tau (type_arrow type_nativeint (type_arrow type_nativeint type_bool)) then
        Pbintcmp(Pnativeint, cmp)
      else
        Pgencmp(cmp)
  with
    | Not_found ->
        try
          Hashtbl.find primitives prim.prim_name
        with
          | Not_found ->
              Pextcall(prim)


(**********************************)
(*** Translation of expressions ***)
(**********************************)

let rec translate_exp exp =
  match exp.exp_desc with
    | Texp_constant(c) ->
        Lconst(Sconst_base(c))
    | Texp_ident(id, { val_kind = Val_regular }) ->
        (* TODO - globals? *)
        Lident(id)
    | Texp_ident(_, { val_kind = Val_primitive(prim) }) ->
        let p = translate_primitive exp.exp_gamma prim exp.exp_tau in
        let idl = ListUtils.init prim.prim_arity Ident.create_tmp in
          Lfunction(idl, Lprim(p, List.map (fun id -> Lident(id)) idl))
    | Texp_let(NonRecursive, cases, exp) ->
        assert false (* TODO *)
    | Texp_let(Recursive, cases, exp) ->
        assert false (* TODO *)
    | Texp_function(cases) ->
        assert false (* TODO *)
    | Texp_apply({ exp_desc = Texp_ident(_, { val_kind = Val_primitive(prim) }) } as exp, expl) when prim.prim_arity = List.length expl ->
        Lprim(translate_primitive exp.exp_gamma prim exp.exp_tau, translate_exp_list expl)
    | Texp_apply(exp, expl) ->
        assert false (* TODO *)
    | Texp_match(exp, cases) ->
        assert false (* TODO *)
    | Texp_try(exp, cases) ->
        assert false (* TODO *)
    | Texp_tuple(expl) ->
        let lambdal = translate_exp_list expl in
          begin try
            Lconst(Sconst_block(0, List.map extract_constant lambdal))
          with
            | Not_constant -> Lprim(Pmakeblock(0), lambdal)
          end
    | Texp_construct(cstr, expl) ->
        let lambdal = translate_exp_list expl in
          begin match cstr.cstr_tag with
            | Cstr_constant(tag) ->
                Lconst(Sconst_pointer(tag))
            | Cstr_block(tag) ->
                begin try
                  Lconst(Sconst_block(tag, List.map extract_constant lambdal))
                with
                  | Not_constant -> Lprim(Pmakeblock(0), lambdal)
                end
            | Cstr_exception(id) ->
                Lprim(Pmakeblock(0), (Lident(id)) :: lambdal)
          end
    | Texp_ifthenelse(exp0, exp1, None) ->
        Lifthenelse(translate_exp exp0, translate_exp exp1, lambda_unit)
    | Texp_ifthenelse(exp0, exp1, Some(exp2)) ->
        Lifthenelse(translate_exp exp0, translate_exp exp1, translate_exp exp2)
    | Texp_sequence(exp1, exp2) ->
        Lsequence(translate_exp exp1, translate_exp exp2)
    | Texp_when(exp1, exp2) ->
        Lifthenelse(translate_exp exp1, translate_exp exp2, Lstaticraise)

and translate_exp_list expl =
  List.map translate_exp expl

