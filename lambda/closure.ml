open Astcommon
open Lambda

module IdentMap = Map.Make(Ident)

(* Approximation of values at compile time *)
type approximation =
  | Approx_unknown
  | Approx_closure of function_description * approximation

and function_description =
    { fun_label:          Ident.t;
      fun_arity:          int;
      mutable fun_closed: bool }


(*********************)
(*** Miscellaneous ***)
(*********************)

let build_apply_function arity =
  let argl = ListUtils.init arity Ident.create_tmp in
  let clos = Ident.create "clos" in
  let rec apply clos n =
    if n = arity - 1 then begin
      Lapply(Lprim(Pfield(0), [Lident(clos)]),
             [Lident(List.nth argl n); Lident(clos)])
    end else begin
      let nclos = Ident.create "clos" in
        Llet(nclos,
             Lapply(Lprim(Pfield(0), [Lident(clos)]),
                    [Lident(List.nth argl n); Lident(clos)]),
             apply nclos (n + 1))
    end in
  let idl = argl @ [clos] in
    Lfunction(idl,
              if arity = 1 then begin
                apply clos 0 
              end else begin
                Lifthenelse(Lprim(Paddrcmp(Ceq),
                                  [Lprim(Pfield(1), [Lident(clos)]);
                                   Lconst(Sconst_base(Const_int(arity)))]),
                            Lapply(Lprim(Pfield(2), [Lident(clos)]),
                                   List.map (fun id -> Lident(id)) idl),
                            apply clos 0)
              end)

let build_apply toplevel arity =
  let apply = Ident.create_predefined ("apply" ^ (string_of_int arity)) in
    if not (List.exists (fun (id, _) -> Ident.equal id apply) !toplevel) then begin
      toplevel := (apply, build_apply_function arity) :: !toplevel;
    end;
    apply

let rec build_curry_functions toplevel arity num =
  let arg = Ident.create "arg" in
  let clos = Ident.create "clos" in
    if num = arity - 1 then begin
      let rec curry args clos n =
        if n = 0 then begin
          Lapply(Lprim(Pfield(2), [Lident(clos)]),
                 args @ [Lident(arg); Lident(clos)])
        end else begin
          let nclos = Ident.create "clos" in
            Llet(nclos,
                 Lprim(Pfield(3), [Lident(clos)]),
                 curry (Lprim(Pfield(2), [Lident(clos)]) :: args) nclos (n - 1))
        end
      in
        toplevel := (Ident.create_predefined ("curry" ^ (string_of_int arity) ^ "_" ^ (string_of_int num)),
                     Lfunction([arg; clos], (curry [] clos (arity - 1)))) :: !toplevel
    end else begin
      let name1 = "curry" ^ (string_of_int arity) in
      let name2 = if num = 0 then name1 else name1 ^ "_" ^ (string_of_int num) in
        toplevel := (Ident.create_predefined name2,
                     Lfunction([arg; clos],
                               Lprim(Pmakeblock(Lambda.make_header Lambda.tag_closure 4, Immutable),
                                     [Lident(Ident.create_predefined (name1 ^ "_" ^ (string_of_int (num + 1))));
                                      Lconst(Sconst_base(Const_int(1)));
                                      Lident(arg);
                                      Lident(clos)]))) :: !toplevel;
        build_curry_functions toplevel arity (num + 1)
    end

let build_curry toplevel arity =
  let curry = Ident.create_predefined ("curry" ^ (string_of_int arity)) in
    if not (List.exists (fun (id, _) -> Ident.equal id curry) !toplevel) then begin
      build_curry_functions toplevel arity 0
    end;
    curry
  
let build_closure toplevel fundesc =
  assert (fundesc.fun_arity > 0);
  (* generate a trampoline with env if the function is closed *)
  let label = (if fundesc.fun_closed then begin
                 let label = Ident.create ((Ident.name fundesc.fun_label) ^ "_trampoline") in
                 let env = Ident.create "env" in
                 let idl = ListUtils.init fundesc.fun_arity Ident.create_tmp in
                   toplevel := (label,
                                Lfunction(idl @ [env],
                                          Lapply(Lident(fundesc.fun_label),
                                                 List.map (fun id -> Lident(id)) idl))) :: !toplevel;
                   label
               end else begin
                 fundesc.fun_label
               end) in
  if fundesc.fun_arity = 1 then begin
    [Lconst(Sconst_pointer(Lambda.make_header Lambda.tag_closure 1 (* TODO *)));
     Lident(label);
     Lconst(Sconst_base(Const_int(fundesc.fun_arity)))]
  end else begin
    [Lconst(Sconst_pointer(Lambda.make_header Lambda.tag_closure 1 (* TODO *)));
     Lident(build_curry toplevel fundesc.fun_arity);
     Lconst(Sconst_base(Const_int(fundesc.fun_arity)));
     Lident(label)]
  end

let build_offset n lambda =
  if n = 0 then
    lambda
  else
    Lprim(Paddaddr, [lambda; Lconst(Sconst_base(Const_int(n)))])

(* TODO *)
let rec is_pure = function
  | Lconst(_)
  | Lident(_) -> true
  | Lapply(_)
  | Lfunction(_) -> false
  | Llet(_, lambda1, lambda2) -> is_pure lambda1 && is_pure lambda2
  | Lletrec(_)
  | Lprim((Praise | Pextcall(_)), _) -> false
  | Lprim(_, lambdal) -> List.for_all is_pure lambdal
  | Lswitch(_)
  | Lstaticraise
  | Lstaticcatch(_)
  | Ltrywith(_) -> false
  | Lifthenelse(lambda0, lambda1, lambda2) -> is_pure lambda0 && is_pure lambda1 && is_pure lambda2
  | Lsequence(lambda1, lambda2) -> is_pure lambda1 && is_pure lambda2

let build_direct_apply fundesc lambda lambdal =
  let lambdal = (if fundesc.fun_closed then
                   lambdal
                 else
                   lambdal @ [lambda]) in
  let lapply = Lapply(Lident(fundesc.fun_label), lambdal) in
    (* if lambda is not pure then we need to evaluate it first *)
    if not fundesc.fun_closed || is_pure lambda then
      lapply
    else
      Lsequence(lambda, lapply)

let rec build_generic_apply toplevel lambda lambdal =
  match lambda, lambdal with
    | Lident(_) as lid, [lambda] ->
        Lapply(Lprim(Pfield(0), [lid]),
               [lambda; lid])
    | lambda, ([_] as lambdal) ->
        let id = Ident.create "clos" in
          Llet(id,
               lambda,
               build_generic_apply toplevel (Lident(id)) lambdal)
    | lambda, lambdal ->
        Lapply(Lident(build_apply toplevel (List.length lambdal)),
               lambdal @ [lambda])
      

(**************************)
(*** Closure conversion ***)
(**************************)

let rec close toplevel aenv cenv = function
  | Lconst(_) as lambda ->
      lambda, Approx_unknown
  | Lident(id) as lambda ->
      begin match (try IdentMap.find id aenv with Not_found -> Approx_unknown) with
        | approx ->
            (try IdentMap.find id cenv with Not_found -> lambda), approx
      end
  | Lapply(lambda, lambdal) ->
      let lambdal = List.map (fun lambda -> fst (close toplevel aenv cenv lambda)) lambdal in
        begin match close toplevel aenv cenv lambda with
          | lambda, Approx_closure(fundesc, approx) when fundesc.fun_arity = List.length lambdal ->
              build_direct_apply fundesc lambda lambdal, approx
          | lambda, Approx_closure(fundesc, approx) when fundesc.fun_arity < List.length lambdal ->
              let lambdal1, lambdal2 = ListUtils.split fundesc.fun_arity lambdal in
                build_generic_apply toplevel (build_direct_apply fundesc lambda lambdal1) lambdal2, Approx_unknown
          | lambda, _ ->
              build_generic_apply toplevel lambda lambdal, Approx_unknown
        end
  | Lfunction(_) as lambda ->
      let id = Ident.create "fun" in
        close toplevel aenv cenv (Lletrec([id, lambda], Lident(id)))
  | Llet(id, (Lfunction(_) as lambda1), lambda2) ->
      close toplevel aenv cenv (Lletrec([id, lambda1], lambda2))
  | Llet(id, lambda1, lambda2) ->
      let lambda1, approx1 = close toplevel aenv cenv lambda1 in
      let lambda2, approx2 = close toplevel (IdentMap.add id approx1 aenv) cenv lambda2 in
        Llet(id, lambda1, lambda2), approx2
  | Lletrec(idlambdal, lambda) ->
      close_letrec toplevel aenv cenv idlambdal lambda
  | Lprim(prim, lambdal) ->
      let lambdal = List.map (fun lambda -> fst (close toplevel aenv cenv lambda)) lambdal in
        Lprim(prim, lambdal), Approx_unknown
  | Lswitch(lambda, switch) ->
      let lambda, _ = close toplevel aenv cenv lambda in
        Lswitch(lambda,
                { switch with
                    sw_consts = (List.map
                                   (fun (tag, lambda) -> tag, fst (close toplevel aenv cenv lambda))
                                   switch.sw_consts);
                    sw_blocks = (List.map
                                   (fun (tag, lambda) -> tag, fst (close toplevel aenv cenv lambda))
                                   switch.sw_blocks);
                    sw_default = (match switch.sw_default with
                                    | None -> None
                                    | Some(lambda) -> Some(fst (close toplevel aenv cenv lambda))) }), Approx_unknown
  | Lstaticraise ->
      Lstaticraise, Approx_unknown
  | Lstaticcatch(lambda1, lambda2) ->
      let lambda1, _ = close toplevel aenv cenv lambda1 in
      let lambda2, _ = close toplevel aenv cenv lambda2 in
        Lstaticcatch(lambda1, lambda2), Approx_unknown
  | Ltrywith(lambda1, id, lambda2) ->
      let lambda1, _ = close toplevel aenv cenv lambda1 in
      let lambda2, _ = close toplevel aenv cenv lambda2 in
        Ltrywith(lambda1, id, lambda2), Approx_unknown
  | Lifthenelse(lambda0, lambda1, lambda2) ->
      let lambda0, _ = close toplevel aenv cenv lambda0 in
      let lambda1, _ = close toplevel aenv cenv lambda1 in
      let lambda2, _ = close toplevel aenv cenv lambda2 in
        Lifthenelse(lambda0, lambda1, lambda2), Approx_unknown
  | Lsequence(lambda1, lambda2) ->
      let lambda1, _ = close toplevel aenv cenv lambda1 in
      let lambda2, approx2 = close toplevel aenv cenv lambda2 in
        Lsequence(lambda1, lambda2), approx2
        
and close_letrec toplevel aenv cenv idlambdal lambda =
  let fvl = IdentSet.elements (Lambda.fv (Lletrec(idlambdal, lambda))) in
  let offset = ref (-1) in
  (* Build the function description list from the idlambdal *)
  let fundefl = (List.map
                   (function
                      | (id, Lfunction(idl, lambda)) ->
                          let fundesc = { fun_label = id;
                                          fun_arity = List.length idl;
                                          fun_closed = true } in
                          let off = !offset + 1 in
                            offset := off + 3; (* [trampoline, arity, function pointer] *)
                            id, idl, lambda, fundesc, off
                      | _ ->
                          assert false)
                   idlambdal) in
  (* Offset the free variables *)
  let offset_fv = !offset in
  (* Build an approximate environment for compiling the functions *)
  let aenv_rec = (List.fold_left
                    (fun aenv (id, _, _, fundesc, _) ->
                       IdentMap.add id (Approx_closure(fundesc, Approx_unknown)) aenv)
                    aenv
                    fundefl) in
  (* Assume that the environment is not needed *)
  let closed = ref true in
  (* Translate a single function definition *)
  let close_fundef (id, idl, lambda, fundesc, offset0) =
    let id0 = Ident.create "env" in
    let lid0 = Lident(id0) in
    let cenv =
      let offset = ref (offset_fv - offset0) in
        (List.fold_left
           (fun cenv id ->
              let off = !offset in
                offset := off + 1;
                IdentMap.add id (Lprim(Pfield(off), [lid0])) cenv)
           cenv
           fvl) in
    let cenv = (List.fold_left
                  (fun cenv (id, _, _, _, offset) ->
                     IdentMap.add id (build_offset (offset - offset0) lid0) cenv)
                  cenv
                  fundefl) in
    let lambda, approx = close toplevel aenv_rec cenv lambda in
      (* invalidate the assumption if we actually need the environment *)
      if !closed && Lambda.occur id0 lambda then closed := false;
      (id,
       offset0,
       fundesc,
       Lfunction(idl @ (if fundesc.fun_closed then [] else [id0]), lambda),
       Approx_closure(fundesc, approx)) in
  let closl = (begin
                 (* try to close the functions under the assumption that all of them are closed *)
                 let closl = List.map close_fundef fundefl in
                   (* check if the assumption was invalidated *)
                   if not (!closed) then begin
                     (* do it again with the environment this time *)
                     List.iter (fun (_, _, _, fundesc, _) -> fundesc.fun_closed <- false) fundefl;
                     List.map close_fundef fundefl
                   end else begin
                     closl
                   end
               end) in
  let id0 = Ident.create "clos" in
  let lid0 = Lident(id0) in
  let aenv_body = ref aenv in
  let cenv_body = ref cenv in
  let cblock = ref [] in
    (List.iter 
       (fun (id, offset, fundesc, lambda, approx) ->
          toplevel := (id, lambda) :: !toplevel;
          aenv_body := IdentMap.add id approx !aenv_body;
          cenv_body := IdentMap.add id (build_offset offset lid0) !cenv_body;
          cblock := !cblock @ (build_closure toplevel fundesc))
       closl);
    let lambda, approx = close toplevel !aenv_body !cenv_body lambda in
      begin match !cblock @ (List.map (fun id -> Lident(id)) fvl) with
        | _ when not (IdentSet.mem id0 (Lambda.fv lambda)) ->
            lambda, approx
        | Lconst(Sconst_pointer(header)) :: lambdal ->
            let tag, _ = Lambda.split_header header in
            let header = Lambda.make_header tag (List.length lambdal) in
              Llet(id0, Lprim(Pmakeblock(header, Immutable), lambdal), lambda), approx
        | _ ->
            assert false
      end

let close_lambda lambda =
  let toplevel = ref [] in
  let lambda, _ = close toplevel IdentMap.empty IdentMap.empty lambda in
    if !toplevel <> [] then
      Lletrec(!toplevel, lambda)
    else
      lambda
