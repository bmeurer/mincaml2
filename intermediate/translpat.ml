open Astcommon
open Lambda
open Primitive
open Typedast
open Types

type matching =
  | Matching of (pattern list * lambda) list * Ident.t list

let wildcard pat = { pat with pat_desc = Tpat_any }

let rec flatten (Matching(clausel, idl)) =
  let rec flatten_clause idl (patl, lambda) =
    match idl, patl with
      | [], [] ->
          [], lambda
      | id :: idl, ({ pat_desc = Tpat_ident(id') } as pat) :: patl ->
          let patl, lambda = flatten_clause idl (patl, lambda) in
            wildcard pat :: patl, Lambda.subst id' (Lident(id)) lambda
      | (id :: _) as idl, ({ pat_desc = Tpat_alias(pat, id') }) :: patl ->
          let patl, lambda = flatten_clause idl (pat :: patl, lambda) in
            patl, Lambda.subst id' (Lident(id)) lambda
      | _ :: idl, pat :: patl ->
          let patl, lambda = flatten_clause idl (patl, lambda) in
          pat :: patl, lambda
      | _ ->
          assert false
  and flatten_clausel = function
    | [] ->
        []
    | ({ pat_desc = Tpat_or(pat1, pat2) } :: patl, lambda) :: clausel ->
        flatten_clausel ((pat1 :: patl, lambda) :: (pat2 :: patl, lambda) :: clausel)
    | clause :: clausel ->
        flatten_clause idl clause :: flatten_clausel clausel
  in Matching(flatten_clausel clausel, idl)

let rec expand_tuple = function
  | [] ->
      []
  | ({ pat_desc = Tpat_tuple(patl') } :: patl, lambda) :: clausel ->
      (patl' @ patl, lambda) :: expand_tuple clausel
  | ({ pat_desc = Tpat_any; pat_tau = { typ_desc = Ttuple(taul) } } as pat :: patl, lambda) :: clausel ->
      let patl' = List.rev_map (fun tau -> { pat with pat_desc = Tpat_any; pat_tau = tau }) taul in
        (List.rev_append patl' patl, lambda) :: expand_tuple clausel
  | _ ->
      assert false

let rec partition_constant c = function
  | ({ pat_desc = Tpat_constant(c') } :: patl, lambda as clause) :: clausel ->
      let clausel1, clausel2 = partition_constant c clausel in
        if c = c' then
          (patl, lambda) :: clausel1, clausel2
        else
          clausel1, clause :: clausel2
  | clausel ->
      [], clausel

let rec partition_constructors = function
  | ({ pat_desc = Tpat_construct(cstr, patl') } :: patl, lambda) :: clausel ->
      let constl, blockl, clausel = partition_constructors clausel in
        assert (cstr.cstr_arity = List.length patl');
        begin match cstr.cstr_tag with
          | Cstr_constant(tag) ->
              begin try
                assert (cstr.cstr_arity = 0);
                let case = List.assoc (tag, cstr.cstr_arity) constl in
                  case := (patl, lambda) :: !case;
                  constl, blockl, clausel
              with
                | Not_found -> ((tag, cstr.cstr_arity), ref [patl, lambda]) :: constl, blockl, clausel
              end
          | Cstr_block(tag) ->
              begin try
                assert (cstr.cstr_arity > 0);
                let case = List.assoc (tag, cstr.cstr_arity) blockl in
                  case := (patl' @ patl, lambda) :: !case;
                  constl, blockl, clausel
              with
                | Not_found -> constl, ((tag, cstr.cstr_arity), ref [patl' @ patl, lambda]) :: blockl, clausel
              end
          | _ -> (* handled elsewhere *)
              assert false
        end
  | clausel ->
      [], [], clausel

let rec partition_exception id = function
  | ({ pat_desc = Tpat_construct({ cstr_tag = Cstr_exception(id') }, patl') } :: patl, lambda as clause) :: clausel ->
      let clausel1, clausel2 = partition_exception id clausel in
        if id = id' then
          (patl' @ patl, lambda) :: clausel1, clausel2
        else
          clausel1, clause :: clausel2
  | clausel ->
      [], clausel

let rec partition_wildcard = function
  | ({ pat_desc = Tpat_any } :: patl, lambda) :: clausel ->
      let clausel1, clausel2 = partition_wildcard clausel in
        (patl, lambda) :: clausel1, clausel2
  | clausel ->
      [], clausel

let rec compile matching =
  match flatten matching with
    (* Case 1: Empty matrix, no match *)
    | Matching([], _) ->
        Lstaticraise, false

    (* Case 2: No columns, guarded action *)
    | Matching(([], Lifthenelse(lambda0, lambda1, Lstaticraise)) :: clausel, idl) ->
        let lambda2, total = compile (Matching(clausel, idl)) in
          Lifthenelse(lambda0, lambda1, lambda2), total

    (* Case 3: No columns, unguarded action *)
    | Matching(([], lambda) :: _, _) ->
        lambda, true

    (* Case 4: Column 0 starts with wildcard *)
    | Matching(({ pat_desc = Tpat_any } :: patl, lambda) :: clausel, (_ :: idl1 as idl)) ->
        let clausel1, clausel2 = partition_wildcard clausel in
        let lambda1, total1 = compile (Matching((patl, lambda) :: clausel1, idl1)) in
        let lambda2, total2 = compile (Matching(clausel2, idl)) in
          if total1 then
            lambda1, true
          else
            Lstaticcatch(lambda1, lambda2), total2

    (* Case 5: Column 0 starts with tuple pattern *)
    | Matching(({ pat_desc = Tpat_tuple(patl') } :: _, _) :: _ as clausel, id0 :: idl1) ->
        let lid0 = Lident(id0) in
        let id0il = ListUtils.rev_init (List.length patl') (fun i -> Ident.create_tmp i, i) in
        let clausel' = expand_tuple clausel in
        let lambda, total = compile (Matching(clausel', (List.rev_map fst id0il) @ idl1)) in
          (List.fold_left
             (fun lambda (id, i) -> Llet(id, Lprim(Pgetfield(i), [lid0]), lambda))
             lambda
             id0il), total

    (* Case 6: Column 0 starts with a constant pattern *)
    | Matching(({ pat_desc = Tpat_constant(c) } :: patl, lambda) :: clausel, (id0 :: idl1 as idl)) ->
        let clausel1, clausel2 = partition_constant c clausel in
        let lambda1, total1 = compile (Matching((patl, lambda) :: clausel1, idl1)) in
        let lambda2, total2 = compile (Matching(clausel2, idl)) in
        let lambda0 = (match c with
                         | Const_int(_)
                         | Const_char(_) ->
                             Lprim(Pintcmp(Ceq), [Lident(id0); Lconst(Sconst_base(c))])
                         | Const_float(_) ->
                             Lprim(Pfloatcmp(Ceq), [Lident(id0); Lconst(Sconst_base(c))])
                         | Const_int32(_) ->
                             Lprim(Pbintcmp(Pint32, Ceq), [Lident(id0); Lconst(Sconst_base(c))])
                         | Const_int64(_) ->
                             Lprim(Pbintcmp(Pint64, Ceq), [Lident(id0); Lconst(Sconst_base(c))])
                         | Const_nativeint(_) ->
                             Lprim(Pbintcmp(Pnativeint, Ceq), [Lident(id0); Lconst(Sconst_base(c))])
                         | Const_string(_) ->
                             Lprim(Pintcmp(Ceq), [Lconst(Sconst_base(Const_int(0)));
                                                  Lprim(Pcompare, [Lident(id0); Lconst(Sconst_base(c))])])) in
          if total1 then
            Lifthenelse(lambda0, lambda1, lambda2), total2
          else
            Lstaticcatch(Lifthenelse(lambda0, lambda1, Lstaticraise), lambda2), total2

    (* Case 7: Column 0 starts with an exception constructor pattern *)
    | Matching(({ pat_desc = Tpat_construct({ cstr_tag = Cstr_exception(id) }, patl') } :: patl, lambda) :: clausel, (id0 :: idl1 as idl)) ->
        let lid0 = Lident(id0) in
        let id0il = ListUtils.rev_init (List.length patl') (fun i -> Ident.create_tmp i, i) in
        let clausel1, clausel2 = partition_exception id clausel in
        let clausel1 = (patl' @ patl, lambda) :: clausel1 in
        let lambda1, total1 = compile (Matching(clausel1, (List.rev_map fst id0il) @ idl1)) in
        let lambda2, total2 = compile (Matching(clausel2, idl)) in
        let lambda1 = (List.fold_left
                         (fun lambda (id, i) -> Llet(id, Lprim(Pgetfield(i + 1), [lid0]), lambda))
                         lambda1
                         id0il) in
        let lambda0 = Lprim(Pintcmp(Lambda.Ceq), [Lident(id); Lprim(Pgetfield(0), [lid0])]) in
          if total1 then
            Lifthenelse(lambda0, lambda1, lambda2), total2
          else
            Lstaticcatch(Lifthenelse(lambda0, lambda1, Lstaticraise), lambda2), total2

    (* Case 8: Column 0 starts with a constructor pattern *)
    | Matching(({ pat_desc = Tpat_construct(cstr, _) } :: _, _) :: _ as clausel, (id0 :: idl1 as idl)) ->
        let lid0 = Lident(id0) in
        let rec compile_casel = function
          | [] ->
              [], true
          | ((tag, arity), clauselref) :: casel ->
              let id0il = ListUtils.rev_init arity (fun i -> Ident.create_tmp i, i) in
              let lambda1, total1 = compile (Matching(!clauselref, (List.rev_map fst id0il) @ idl1)) in
              let lambda1 = (List.fold_left
                               (fun lambda (id, i) -> Llet(id, Lprim(Pgetfield(i), [lid0]), lambda))
                               lambda1
                               id0il) in
              let constrl2, total2 = compile_casel casel in
                (tag, lambda1) :: constrl2, total1 && total2 in
        let constl, blockl, clausel2 = partition_constructors clausel in
        let consts, consts_total = compile_casel constl in
        let blocks, blocks_total = compile_casel blockl in
        let total1 = consts_total && blocks_total in
        let lambda2, total2 = compile (Matching(clausel2, idl)) in
        let switch = { sw_numconsts = List.length consts;
                       sw_consts = consts;
                       sw_numblocks = List.length blocks;
                       sw_blocks = blocks;
                       sw_default = None } in
          if total1 && cstr.cstr_consts = switch.sw_numconsts && cstr.cstr_blocks = switch.sw_numblocks then
            (* Case 8.1: Spans all constructors and constructor clauses are total *)
            Lswitch(lid0, switch), true
          else if total1 then
            (* Case 8.2: Constructor clauses are total *)
            Lswitch(lid0, { switch with sw_default = Some(lambda2) }), total2
          else if cstr.cstr_consts = switch.sw_numconsts && cstr.cstr_blocks = switch.sw_numblocks then
            (* Case 8.3: Spans all constructors, but constructor clauses aren't total *)
            Lstaticcatch(Lswitch(lid0, switch), lambda2), total2
          else
            (* Case 8.4: Neither of the above *)
            Lstaticcatch(Lswitch(lid0, { switch with sw_default = Some(Lstaticraise) }), lambda2), total2

    (* Should not happen... *)
    | _ ->
        assert false


(*********************)
(*** Miscellaneous ***)
(*********************)

let rec name_pattern default = function
  | (pat :: patl, lambda) :: clausel ->
      begin match pat.pat_desc with
        | Tpat_ident(id)
        | Tpat_alias(_, id) -> id
        | Tpat_or(pat1, pat2) -> name_pattern default ((pat1 :: patl, lambda) :: (pat2 :: patl, lambda) :: clausel)
        | _ -> name_pattern default clausel
      end
  | _ -> Ident.create default

let raise_match_failure loc =
  let pos = loc.Location.loc_start in
  let file = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    Lprim(Praise,
          [Lprim(Pmakeblock(0, Immutable),
                 [Lident(Typeenv.ident_match_failure);
                  Lconst(Sconst_block(0,
                                      [Sconst_base(Const_string(file));
                                       Sconst_base(Const_int(line));
                                       Sconst_base(Const_int(char))]))])])


(********************)
(*** Entry points ***)
(********************)

let rec translate_match failure lambda clausel =
  match lambda with
    | Lident(id) ->
        let lambda, total = compile (Matching(clausel, [id])) in
          if total then
            lambda
          else
            Lstaticcatch(lambda, failure)
    | lambda ->
        let id = name_pattern "t" clausel in
          Llet(id, lambda, translate_match failure (Lident(id)) clausel)

let translate_match_check_failure loc lambda clausel =
  translate_match (raise_match_failure loc) lambda clausel

let translate_tupled_match failure lambdal clausel =
  let rec compile_aux idl = function
    | [] ->
        let Matching(clausel, idl) = flatten (Matching(clausel, List.rev idl)) in
        let lambda, total = compile (Matching(expand_tuple clausel, idl)) in
          if total then
            lambda
          else
            Lstaticcatch(lambda, failure)
    | Lident(id) :: lambdal ->
        compile_aux (id :: idl) lambdal
    | lambda :: lambdal ->
        let id = Ident.create "t" in
          Llet(id, lambda, compile_aux (id :: idl) lambdal)
  in compile_aux [] lambdal

let translate_tupled_match_check_failure loc lambdal clausel =
  translate_tupled_match (raise_match_failure loc) lambdal clausel
