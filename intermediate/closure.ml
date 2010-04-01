open Astcommon
open Lambda

module IdentMap = Map.Make(Ident)

(* Misc *)

let build_offset n lambda =
  if n = 0 then
    lambda
  else
    Lprim(Paddaddr, [lambda; Lconst(Sconst_base(Const_int(n)))])

(* Approximation of values at compile time *)
type approximation =
  | Approx_unknown
  | Approx_closure of function_description * approximation

and function_description =
    { fun_label: Ident.t;
      fun_arity: int }

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
        (* TODO *)
        begin match close toplevel aenv cenv lambda with
          | lambda, Approx_closure(fundesc, approx) when fundesc.fun_arity = List.length lambdal ->
              Lapply(Lident(fundesc.fun_label), lambda :: lambdal), approx
          | lambda, _ ->
              let id = Ident.create "clos" in
              let lid = Lident(id) in
                Llet(id, lambda, Lapply(Lprim(Pgetfield(0), [lid]), lid :: lambdal)), Approx_unknown
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
                                          fun_arity = List.length idl } in
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
                IdentMap.add id (Lprim(Pgetfield(off), [lid0])) cenv)
           cenv
           fvl) in
    let cenv = (List.fold_left
                  (fun cenv (id, _, _, _, offset) ->
                     IdentMap.add id (build_offset (offset - offset0) lid0) cenv)
                  cenv
                  fundefl) in
    let lambda, approx = close toplevel aenv_rec cenv lambda in
      id, offset0, Lfunction(id0 :: idl, lambda), Approx_closure(fundesc, approx) in
  let closl = List.map close_fundef fundefl in
  let id0 = Ident.create "clos" in
  let lid0 = Lident(id0) in
  let aenv_body = ref aenv in
  let cenv_body = ref cenv in
    (List.iter 
       (fun (id, offset, lambda, approx) ->
          toplevel := (id, lambda) :: !toplevel;
          aenv_body := IdentMap.add id approx !aenv_body;
          cenv_body := IdentMap.add id (build_offset offset lid0) !cenv_body)
       closl);
    let lambda, approx = close toplevel !aenv_body !cenv_body lambda in
      Llet(id0, Lconst(Sconst_base(Const_string("TODO_closure"))), lambda), approx

let close_lambda lambda =
  let toplevel = ref [] in
  let lambda, _ = close toplevel IdentMap.empty IdentMap.empty lambda in
    Lletrec(!toplevel, lambda)
