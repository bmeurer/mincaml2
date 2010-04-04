open Astcommon

type comparison =
  | Ceq | Cne
  | Clt | Cgt
  | Cle | Cge

and boxed_integer =
  | Pint32
  | Pint64
  | Pnativeint

and primitive =
  (* Miscellaneous *)
  | Pignore
  | Pidentity
  | Praise
  | Pcompare
  (* Operations on globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of nativeint * mutable_flag
  | Pfield of int
  | Poffset of int
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

and structured_constant =
  | Sconst_base of constant
  | Sconst_pointer of nativeint
  | Sconst_immstring of string
  | Sconst_block of nativeint * structured_constant list

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

let tag_closure = 252
let tag_float = 253
let tag_string = 254
let tag_custom = 255

let lambda_unit = Lconst(Sconst_base(Const_int(0)))

let rec translate_ident = function
  | id when Ident.is_persistent id ->
      Lprim(Pgetglobal id, [])
  | id -> Lident(id)

let make_header tag wosize =
  assert (tag >= 0 && tag < (1 lsl 8));
  assert (wosize > 0 && wosize < (1 lsl ((Sys.word_size - 1) * 8)) - 1);
  let tag = Nativeint.of_int tag in
  let wosize = Nativeint.of_int (wosize + 1) in
    Nativeint.logor (Nativeint.shift_left wosize 8) tag

let split_header header =
  let tag = Nativeint.logand header 0xffn in
  let wosize = Nativeint.shift_right header 8 in
    Nativeint.to_int tag, Nativeint.to_int wosize

let fv lambda =
  let fvs = ref IdentSet.empty in
  let rec fv = function
    | Lconst(_) ->
        ()
    | Lident(id) ->
        fvs := IdentSet.add id !fvs;
    | Lapply(lambda, lambdal) ->
        fv lambda; 
        List.iter fv lambdal
    | Lfunction(idl, lambda) ->
        fv lambda;
        List.iter (fun id -> fvs := IdentSet.remove id !fvs) idl
    | Llet(id, lambda1, lambda2) ->
        fv lambda1;
        fv lambda2;
        fvs := IdentSet.remove id !fvs
    | Lletrec(idlambdal, lambda) ->
        fv lambda;
        List.iter (fun (_, lambda) -> fv lambda) idlambdal;
        List.iter (fun (id, _) -> fvs := IdentSet.remove id !fvs) idlambdal
    | Lprim(_, lambdal) ->
        List.iter fv lambdal
    | Lswitch(lambda, switch) ->
        fv lambda;
        List.iter (fun (_, lambda) -> fv lambda) switch.sw_consts;
        List.iter (fun (_, lambda) -> fv lambda) switch.sw_blocks;
        (match switch.sw_default with None -> () | Some(lambda) -> fv lambda)
    | Lstaticraise ->
        ()
    | Lstaticcatch(lambda1, lambda2) ->
        fv lambda1;
        fv lambda2
    | Ltrywith(lambda1, id, lambda2) ->
        fv lambda1;
        fv lambda2;
        fvs := IdentSet.remove id !fvs
    | Lifthenelse(lambda0, lambda1, lambda2) ->
        fv lambda0;
        fv lambda1;
        fv lambda2
    | Lsequence(lambda1, lambda2) ->
        fv lambda1;
        fv lambda2
  in fv lambda; !fvs

let occur id lambda =
  IdentSet.mem id (fv lambda)

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
