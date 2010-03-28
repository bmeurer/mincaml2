open Astcommon

type comparison =
  | Ceq | Cne
  | Clt | Cgt
  | Cle | Cge

and primitive =
  (* Miscellaneous *)
  | Pignore
  (* Boxed object operations *)
  | Pgetfield of int
  (* Integer operations *)
  | Pnegint
  | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint | Plslint | Plsrint | Pasrint
  | Pintcmp of comparison
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcmp of comparison
  
and structured_constant =
  | Sconst_base of constant
  | Sconst_pointer of int
  | Sconst_block of int * constant list

and lambda =
  | Lconst of structured_constant
  | Lident of Ident.t
  | Llet of Ident.t * lambda * lambda
  | Lprim of primitive * lambda list
  | Lcond of lambda * lambda_cond
  | Lswitch of lambda * lambda_switch
  | Lstaticraise
  | Lstaticcatch of lambda * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda

and lambda_cond =
    { cond_numcases: int;
      cond_cases: (constant * lambda) list;
      cond_default: lambda }

and lambda_switch =
    { sw_numconsts: int;
      sw_consts: (int * lambda) list;
      sw_numblocks: int;
      sw_blocks: (int * lambda) list;
      sw_default: lambda option }

let lambda_unit = Lconst(Sconst_pointer 0)

let rec subst id' lambda' = function
  | Lconst(_) as lambda ->
      lambda
  | Lident(id) when Ident.equal id id' ->
      lambda'
  | Lident(_) as lambda ->
      lambda
  | Llet(id, lambda1, lambda2) when Ident.equal id id' ->
      Llet(id, subst id' lambda' lambda1, lambda2)
  | Llet(id, lambda1, lambda2) ->
      Llet(id, subst id' lambda' lambda1, subst id' lambda' lambda2)
  | Lprim(prim, lambdal) ->
      Lprim(prim, List.map (subst id' lambda') lambdal)
  | Lcond(lambda, cond) ->
      Lcond(subst id' lambda' lambda,
            { cond with
                cond_cases = List.map (fun (c, lambda) -> c, subst id' lambda' lambda) cond.cond_cases;
                cond_default = subst id' lambda' cond.cond_default })
  | Lswitch(lambda, switch) ->
      Lswitch(subst id' lambda' lambda,
              { switch with
                  sw_consts = List.map (fun (i, lambda) -> i, subst id' lambda' lambda) switch.sw_consts;
                  sw_blocks = List.map (fun (i, lambda) -> i, subst id' lambda' lambda) switch.sw_blocks;
                  sw_default = (match switch.sw_default with
                                  | None -> None
                                  | Some(lambda) -> Some(subst id' lambda' lambda)) })
  | Lstaticraise as lambda ->
      lambda
  | Lstaticcatch(lambda1, lambda2) ->
      Lstaticcatch(subst id' lambda' lambda1,
                   subst id' lambda' lambda2)
  | Lifthenelse(lambda0, lambda1, lambda2) ->
      Lifthenelse(subst id' lambda' lambda0,
                  subst id' lambda' lambda1,
                  subst id' lambda' lambda2)
  | Lsequence(lambda1, lambda2) ->
      Lsequence(subst id' lambda' lambda1,
                subst id' lambda' lambda2)
