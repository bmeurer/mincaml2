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
  
and constant =
  | Const_base of Astcommon.constant
  | Const_pointer of int
  | Const_box of int * constant list

and lambda =
  | Lident of Ident.t
  | Lconst of constant
  | Lprimitive of primitive * lambda list
  | Lswitch of lambda * lambda_switch
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * int * Ident.t list * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda

and lambda_switch =
    { sw_numconsts: int;
      sw_consts: (int * lambda) list;
      sw_numboxed: int;
      sw_boxed: (int * lambda) list;
      sw_default: lambda option }

val lambda_unit: lambda
