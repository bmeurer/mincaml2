open Astcommon
open Format
open Lambda
open Primitive
open Translpat
open Typedast
open Typeenv
open Types
open Typing


(***********************)
(*** Error reporting ***)
(***********************)

type error =
  | Illegal_letrec_expression
  | Illegal_letrec_pattern

exception Error of error * Location.t

let report_error ppf = function
  | Illegal_letrec_expression ->
      fprintf ppf "This kind of expression is not allowed as right-hand side of `let rec'"
  | Illegal_letrec_pattern ->
      fprintf ppf "Only variables are allowed as left-hand side of `let rec'"


(*********************)
(*** Miscellaneous ***)
(*********************)

exception Not_constant

let extract_constant = function
  | Lconst(sc) -> sc
  | _ -> raise Not_constant

let rec name_pattern default = function
  | (pat, exp) :: casel ->
      begin match pat.pat_desc with
        | Tpat_ident(id)
        | Tpat_alias(_, id) -> id
        | Tpat_or(pat1, pat2) -> name_pattern default ((pat1, exp) :: (pat2, exp) :: casel)
        | _ -> name_pattern default casel
      end
  | _ -> Ident.create default


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
    "%identity",  Pidentity;
    "%ignore",    Pignore;
    "%raise",     Praise;
    "%getfield0", Pgetfield(0);
    "%getfield1", Pgetfield(1);
    "%eq",        Pintcmp(Ceq);
    "%noteq",     Pintcmp(Cne);
    "%negint",    Pnegint;
    "%addint",    Paddint;
    "%subint",    Psubint;
    "%mulint",    Pmulint;
    "%divint",    Pdivint;
    "%modint",    Pmodint;
    "%andint",    Pandint;
    "%orint",     Porint;
    "%xorint",    Pxorint;
    "%lslint",    Plslint;
    "%lsrint",    Plsrint;
    "%asrint",    Pasrint;
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
        Lident(id)
    | Texp_ident(_, { val_kind = Val_primitive(prim) }) ->
        let p = translate_primitive exp.exp_gamma prim exp.exp_tau in
        let idl = ListUtils.init prim.prim_arity Ident.create_tmp in
          Lfunction(idl, Lprim(p, List.map (fun id -> Lident(id)) idl))
    | Texp_let(rec_flag, casel, exp) ->
        translate_let rec_flag casel (translate_exp exp)
    | Texp_function(casel) ->
        let id = name_pattern "param" casel in
        let lambda = translate_match_check_failure exp.exp_loc (Lident(id)) (translate_casel casel) in
          Lfunction([id], lambda)
    | Texp_apply({ exp_desc = Texp_ident(_, { val_kind = Val_primitive(prim) }) } as exp, expl) when prim.prim_arity = List.length expl ->
        Lprim(translate_primitive exp.exp_gamma prim exp.exp_tau, translate_exp_list expl)
    | Texp_apply(exp, expl) ->
        Lapply(translate_exp exp, List.map translate_exp expl)
    | Texp_match({ exp_desc = Texp_tuple(expl) }, casel) ->
        translate_tupled_match_check_failure exp.exp_loc (translate_exp_list expl) (translate_casel casel)
    | Texp_match(exp', casel) ->
        translate_match_check_failure exp.exp_loc (translate_exp exp') (translate_casel casel)
    | Texp_try(exp', casel) ->
        let id = name_pattern "exn" casel in
        let lid = Lident(id) in
          Ltrywith(translate_exp exp',
                   id,
                   translate_match (Lprim(Praise, [lid])) lid (translate_casel casel))
    | Texp_tuple(expl) ->
        let lambdal = translate_exp_list expl in
          begin try
            Lconst(Sconst_block(0, List.map extract_constant lambdal))
          with
            | Not_constant -> Lprim(Pmakeblock(0, Immutable), lambdal)
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
                  | Not_constant -> Lprim(Pmakeblock(0, Immutable), lambdal)
                end
            | Cstr_exception(id) ->
                Lprim(Pmakeblock(0, Immutable), (Lident(id)) :: lambdal)
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

and translate_casel casel =
  List.map (fun (pat, exp) -> [pat], translate_exp exp) casel

and translate_let rec_flag casel lambda =
  begin match rec_flag with
    | NonRecursive ->
        let rec translate_let_aux = function
          | [] ->
              lambda
          | (pat, exp) :: casel ->
              translate_match_check_failure pat.pat_loc (translate_exp exp) [[pat], translate_let_aux casel]
        in translate_let_aux casel

    | Recursive ->
        let idexpl = (List.map
                        (fun (pat, exp) ->
                           match pat.pat_desc with
                             | Tpat_ident(id) -> id, exp
                             | _ -> raise (Error(Illegal_letrec_pattern, pat.pat_loc)))
                        casel) in
        let translate_letrec_aux (id, exp) =
          match translate_exp exp with
            | Lfunction(_) as lambda -> (id, lambda)
            | _ -> raise (Error(Illegal_letrec_expression, exp.exp_loc))
        in Lletrec(List.map translate_letrec_aux idexpl, lambda)
  end

and translate_structure = function
  | [] ->
      lambda_unit
  | Tstr_exp(exp) :: str ->
      Lsequence(translate_exp exp, translate_structure str)
  | Tstr_let(rec_flag, casel) :: str ->
      translate_let rec_flag casel (translate_structure str)
  | Tstr_typ(_) :: str ->
      translate_structure str
  | Tstr_exn(id, taul) :: str ->
      (* TODO - This let may not be substituted *)
      Llet(id,
           Lprim(Pmakeblock(0, Immutable), [Lconst(Sconst_base(Const_string(Ident.name id)))]),
           translate_structure str)
  | Tstr_external(_) :: str ->
      translate_structure str
