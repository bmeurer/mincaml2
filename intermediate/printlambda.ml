open Astcommon
open Format
open Lambda
open Primitive


let boxed_integer_name = function
  | Pint32 -> "int32"
  | Pint64 -> "int64"
  | Pnativeint -> "nativeint"

let rec print_structured_constant ppf = function
  | Sconst_base(Const_int(n)) ->
      fprintf ppf "%i" n
  | Sconst_base(Const_char(c)) ->
      fprintf ppf "%C" c
  | Sconst_base(Const_float(f)) ->
      fprintf ppf "%s" f
  | Sconst_base(Const_int32(n)) ->
      fprintf ppf "%lil" n
  | Sconst_base(Const_int64(n)) ->
      fprintf ppf "%LiL" n
  | Sconst_base(Const_string(s)) ->
      fprintf ppf "%S" s
  | Sconst_base(Const_nativeint(n)) ->
      fprintf ppf "%nin" n
  | Sconst_block(tag, []) ->
      fprintf ppf "[%i]" tag
  | Sconst_block(tag, sc :: scl) ->
      let print ppf scl = List.iter (fun sc -> fprintf ppf "@ %a" print_structured_constant sc) scl in
        fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag print_structured_constant sc print scl

let print_comparison ppf = function
  | Ceq -> fprintf ppf "="
  | Cne -> fprintf ppf "<>"
  | Clt -> fprintf ppf "<"
  | Cgt -> fprintf ppf ">"
  | Cle -> fprintf ppf "<="
  | Cge -> fprintf ppf ">="

let print_primitive ppf = function
  | Pignore ->
      fprintf ppf "ignore"
  | Pidentity ->
      fprintf ppf "identity"
  | Praise ->
      fprintf ppf "raise"
  | Pcompare ->
      fprintf ppf "compare"
  | Pmakeblock(tag, Mutable) ->
      fprintf ppf "makemutblock %i" tag
  | Pmakeblock(tag, Immutable) ->
      fprintf ppf "makeimmblock %i" tag
  | Pgetfield(n) ->
      fprintf ppf "getfield %i" n
  | Pextcall(prim) ->
      fprintf ppf "%s" prim.prim_name
  | Paddaddr ->
      fprintf ppf "+a"
  | Psubaddr ->
      fprintf ppf "-a"
  | Paddrcmp(cmp) ->
      fprintf ppf "%aa" print_comparison cmp
  | Pnegint ->
      fprintf ppf "~"
  | Paddint ->
      fprintf ppf "+"
  | Psubint ->
      fprintf ppf "-"
  | Pmulint ->
      fprintf ppf "*"
  | Pdivint ->
      fprintf ppf "/"
  | Pmodint ->
      fprintf ppf "mod"
  | Pandint ->
      fprintf ppf "and"
  | Porint ->
      fprintf ppf "or"
  | Pxorint ->
      fprintf ppf "xor"
  | Plslint ->
      fprintf ppf "lsl"
  | Plsrint ->
      fprintf ppf "lsr"
  | Pasrint ->
      fprintf ppf "asr"
  | Pintcmp(cmp) ->
      fprintf ppf "%a" print_comparison cmp
  | Pintoffloat ->
      fprintf ppf "int_of_float"
  | Pfloatofint ->
      fprintf ppf "float_of_int"
  | Pnegfloat ->
      fprintf ppf "~."
  | Paddfloat ->
      fprintf ppf "+."
  | Psubfloat ->
      fprintf ppf "-."
  | Pmulfloat ->
      fprintf ppf "*."
  | Pdivfloat ->
      fprintf ppf "/."
  | Pfloatcmp(cmp) ->
      fprintf ppf "%a." print_comparison cmp
  | Pbintcmp(bi, cmp) ->
      fprintf ppf "%s_%a" (boxed_integer_name bi) print_comparison cmp

let rec print_lambda ppf = function
  | Lconst(sc) ->
      fprintf ppf "%a"
        print_structured_constant sc
  | Lident(id) ->
      fprintf ppf "%a"
        Ident.print id
  | Lapply(lambda, lambdal) ->
      let print_args ppf = List.iter (fun lambda -> fprintf ppf "@ %a" print_lambda lambda) in
        fprintf ppf "@[<2>(apply@ %a%a)@]"
          print_lambda lambda
          print_args lambdal
  | Lfunction(idl, lambda) ->
      let print_ids ppf = List.iter (fun id -> fprintf ppf "@ %a" Ident.print id) in
        fprintf ppf "@[<2>(function%a@ %a)@]"
          print_ids idl
          print_lambda lambda
  | Llet(_) as lambda ->
      let rec bindings = function
        | Llet(id, lambda1, lambda2) ->
            let idlambdal, lambda = bindings lambda2 in
              (id, lambda1) :: idlambdal, lambda
        | lambda ->
            [], lambda
      and print_bindings ppf = 
        let trailing = ref false in
          List.iter (fun (id, lambda) ->
                       if !trailing then fprintf ppf "@ " else trailing := true;
                       fprintf ppf "@[<2>%a@ %a@]" Ident.print id print_lambda lambda)
      in let idlambdal, lambda = bindings lambda in
        fprintf ppf "@[<2>(let@ (@[<hv>%a@])@ %a)@]"
          print_bindings idlambdal
          print_lambda lambda
  | Lletrec(idlambdal, lambda) ->
      let print_bindings ppf =
        let trailing = ref false in
          List.iter (fun (id, lambda) ->
                       if !trailing then fprintf ppf "@ " else trailing := true;
                       fprintf ppf "@[<2>%a@ %a@]" Ident.print id print_lambda lambda)
      in fprintf ppf "@[<2>(letrec@ (@[<hv>%a@])@ %a)@]"
           print_bindings idlambdal
           print_lambda lambda
  | Lprim(p, lambdal) ->
      let print_args ppf = List.iter (fun lambda -> fprintf ppf "@ %a" print_lambda lambda) in
        fprintf ppf "@[<2>(%a%a)@]"
          print_primitive p
          print_args lambdal
  | Lswitch(lambda, switch) ->
      let print_switch ppf switch =
        let trailing = ref false in
          (List.iter
             (fun (tag, lambda) ->
                if !trailing then fprintf ppf "@ " else trailing := true;
                fprintf ppf "@[<hv 1>case const %i:@ %a@]" tag print_lambda lambda)
             switch.sw_consts);
          (List.iter
             (fun (tag, lambda) ->
                if !trailing then fprintf ppf "@ " else trailing := true;
                fprintf ppf "@[<hv 1>case block %i:@ %a@]" tag print_lambda lambda)
             switch.sw_blocks);
          match switch.sw_default with
            | None ->
                ()
            | Some(lambda) ->
                if !trailing then fprintf ppf "@ " else trailing := true;
                fprintf ppf "@[<hv 1>default:@ %a@]" print_lambda lambda
      in fprintf ppf "@[<1>(switch %a@ @[<v 0>%a@])@]"
           print_lambda lambda
           print_switch switch
  | Lstaticraise ->
      fprintf ppf "@[<2>(exit)@]"
  | Lstaticcatch(lambda1, lambda2) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with %a)@]"
        print_lambda lambda1
        print_lambda lambda2
  | Ltrywith(lambda1, id, lambda2) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        print_lambda lambda1
        Ident.print id
        print_lambda lambda2
  | Lifthenelse(lambda0, lambda1, lambda2) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]"
        print_lambda lambda0
        print_lambda lambda1
        print_lambda lambda2
  | Lsequence(lambda1, lambda2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]"
        print_lambda lambda1
        print_lambda lambda2
