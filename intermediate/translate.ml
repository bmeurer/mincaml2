open Lambda
open Primitive
open Typedast
open Types


(* Surround a primitive with a function definition *)
let rec translate_prim prim =
  (* TODO *)
  assert false

let rec translate_prim_applied prim lambdal =
  (* TODO *)
  assert false

let rec translate_exp exp =
  match exp.exp_desc with
    | Texp_constant(constant) ->
        Lconst(Const_base(constant))
    | Texp_ident(id, { val_kind = Val_regular }) ->
        (* TODO - globals? *)
        Lident(id)
    | Texp_ident(id, { val_kind = Val_primitive(prim) }) ->
        assert false (* TODO *)
    | Texp_let(rec_flag, cases, exp) ->
        assert false (* TODO *)
    | Texp_function(cases, partial) ->
        assert false (* TODO *)
    | Texp_apply({ exp_desc = Texp_ident(id, { val_kind = Val_primitive(prim) }) }, expl) when prim.prim_arity = List.length expl ->
        let lambdal = translate_exp_list expl in
          translate_prim_applied prim lambdal
    | Texp_apply(exp, expl) ->
        assert false (* TODO *)
    | Texp_match(exp, cases, partial) ->
        assert false (* TODO *)
    | Texp_try(exp, cases) ->
        assert false (* TODO *)
    | Texp_tuple(expl) ->
        assert false (* TODO *)
    | Texp_construct(id, expl) ->
        assert false (* TODO *)
    | Texp_ifthenelse(exp0, exp1, None) ->
        Lifthenelse(translate_exp exp0, translate_exp exp1, lambda_unit)
    | Texp_ifthenelse(exp0, exp1, Some(exp2)) ->
        Lifthenelse(translate_exp exp0, translate_exp exp1, translate_exp exp2)
    | Texp_sequence(exp1, exp2) ->
        Lsequence(translate_exp exp1, translate_exp exp2)
    | Texp_when(exp1, exp2) ->
        assert false (* TODO *)

and translate_exp_list expl =
  List.map translate_exp expl
