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

let rec subst id lambda lambda' =
  assert false (* TODO *)
