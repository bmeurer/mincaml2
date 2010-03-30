open Astcommon

type comparison =
  | Ceq | Cne
  | Clt | Cgt
  | Cle | Cge
  | Ccmp

and boxed_integer =
  | Pint32
  | Pint64
  | Pnativeint

and primitive =
  (* Miscellaneous *)
  | Pignore
  | Pidentity
  | Praise
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag
  | Pgetfield of int
  (* External call *)
  | Pextcall of Primitive.description
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
  (* Boxed integer operations *)
  | Pbintcmp of boxed_integer * comparison
  (* String operations *)
  | Pstringcmp of comparison
  (* Generic operations *)
  | Pgencmp of comparison

and structured_constant =
  | Sconst_base of constant
  | Sconst_pointer of int
  | Sconst_block of int * structured_constant list

and lambda =
  | Lconst of structured_constant
  | Lident of Ident.t
  | Lapply of lambda * lambda list
  | Lfunction of Ident.t list * lambda
  | Llet of Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list
  | Lswitch of lambda * lambda_switch
  | Lstaticraise
  | Lstaticcatch of lambda * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda

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
  | Lapply(lambda, lambdal) ->
      Lapply(subst id' lambda' lambda, List.map (subst id' lambda') lambdal)
  | Lfunction(idl, _) as lambda when List.mem id' idl ->
      lambda
  | Lfunction(idl, lambda) ->
      Lfunction(idl, subst id' lambda' lambda)
  | Llet(id, lambda1, lambda2) when Ident.equal id id' ->
      Llet(id, subst id' lambda' lambda1, lambda2)
  | Llet(id, lambda1, lambda2) ->
      Llet(id, subst id' lambda' lambda1, subst id' lambda' lambda2)
  | Lletrec(idlambdal, _) as lambda when List.exists (fun (id, _) -> id = id') idlambdal ->
      lambda
  | Lletrec(idlambdal, lambda) ->
      Lletrec(List.map (fun (id, lambda) -> id, subst id' lambda' lambda) idlambdal,
              subst id' lambda' lambda)
  | Lprim(prim, lambdal) ->
      Lprim(prim, List.map (subst id' lambda') lambdal)
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
  | Ltrywith(_, id, _) as lambda when Ident.equal id id' ->
      lambda
  | Ltrywith(lambda1, id, lambda2) ->
      Ltrywith(subst id' lambda' lambda1,
               id,
               subst id' lambda' lambda2)
  | Lifthenelse(lambda0, lambda1, lambda2) ->
      Lifthenelse(subst id' lambda' lambda0,
                  subst id' lambda' lambda1,
                  subst id' lambda' lambda2)
  | Lsequence(lambda1, lambda2) ->
      Lsequence(subst id' lambda' lambda1,
                subst id' lambda' lambda2)
