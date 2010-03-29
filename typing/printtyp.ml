open Format
open Types

let type_names = ref []

let generate_type_name i =
  if i < 26 then String.make 1 (char_of_int (i + 97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i / 26)

let type_name tau =
  begin try
    List.assoc tau !type_names
  with
    | Not_found ->
        let name = generate_type_name (List.length !type_names) in
          type_names := (tau, name) :: !type_names;
          name
  end

let parenthesis needed fmt = if needed then "@[(" ^^ fmt ^^ ")@]" else "@[" ^^ fmt ^^ "@]"

let rec print_type_expr prio ppf tau =
  let tau = repr tau in
    match tau.typ_desc with
      | Tvar(_) ->
          fprintf ppf "'%s" (type_name tau)
      | Tarrow(tau1, tau2) ->
          fprintf ppf (parenthesis (prio >= 1) "%a -> %a")
            (print_type_expr 1) tau1 (print_type_expr 0) tau2
      | Ttuple(taul) ->
          fprintf ppf (parenthesis (prio >= 2) "%a")
            (print_type_list 2 " * ") taul
      | Tconstruct(id, taul) ->
          let rec print_list_aux ppf = function
            | [] -> ()
            | [tau] -> fprintf ppf "%a " (print_type_expr 2) tau
            | taul -> fprintf ppf "(%a) " (print_type_list 0 ", ") taul
          in fprintf ppf "@[%a%s@]" print_list_aux taul (Ident.name id)

and print_type_list prio sep ppf = function
  | [] -> ()
  | [tau] -> print_type_expr prio ppf tau
  | tau :: taul -> fprintf ppf "%a%s%a" (print_type_expr prio) tau sep (print_type_list prio sep) taul

let print_typ ppf tau =
  type_names := [];
  print_type_expr 0 ppf tau

let report_unification_error ppf taupl txt1 txt2 =
  type_names := [];
  match taupl with
    | [] ->
        invalid_arg "Printtyp.report_unification_error"
    | (tau1, tau2) :: _ ->
        fprintf ppf "@[%s@;<1 2>%a@ %s@;<1 2>%a@]"
          txt1 (print_type_expr 0) tau1
          txt2 (print_type_expr 0) tau2
            
