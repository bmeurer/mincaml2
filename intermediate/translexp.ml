open Astcommon
open Lambda
open Primitive
open Typedast
open Types


(****************************)
(*** Constant propagation ***)
(****************************)

exception Not_constant

let propagate_constant = function
  | Lconst(sc) -> sc
  | _ -> raise Not_constant



(* Surround a primitive with a function definition *)
let rec translate_prim prim =
  (* TODO *)
  assert false

(* TODO *)
let rec translate_prim_applied prim lambdal =
  (* TODO *)
  assert false


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
    | Texp_ident(id, { val_kind = Val_primitive(prim) }) ->
        assert false (* TODO *)
    | Texp_let(NonRecursive, cases, exp) ->
        assert false (* TODO *)
    | Texp_let(Recursive, cases, exp) ->
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
        let lambdal = translate_exp_list expl in
          begin try
            Lconst(Sconst_block(0, List.map propagate_constant lambdal))
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
                  Lconst(Sconst_block(tag, List.map propagate_constant lambdal))
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

