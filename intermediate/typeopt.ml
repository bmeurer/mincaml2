open Typedast
open Types

let has_base_type base_typ_id exp =
  let rec is_base_type tau =
    match (repr tau).typ_desc with
      | Tconstruct(id, _) when Ident.equal base_typ_id id ->
          true
      | Tconstruct(id, taul) ->
          begin try
            is_base_type (expand (Typeenv.lookup_type id exp.exp_gamma) taul)
          with
            | Invalid_argument("Types.expand") -> false
          end
      | _ ->
          false
  in is_base_type exp.exp_tau
          

