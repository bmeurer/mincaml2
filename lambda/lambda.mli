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

module IdentSet: Set.S with type elt = Ident.t

val tag_closure: int
val tag_float: int
val tag_string: int
val tag_custom: int

val lambda_unit: lambda

val make_header: int -> int -> nativeint
val split_header: nativeint -> int * int

val fv: lambda -> IdentSet.t
val occur: Ident.t -> lambda -> bool
val subst: Ident.t -> lambda -> lambda -> lambda
