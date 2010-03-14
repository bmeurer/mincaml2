type t =
  | Int of int
  | Char of char
  | Float of float
  | String of string
  | Ident of Id.t
  | If of t * t * t
  | Tuple of t list
  | Sequence of t * t
  | App of t * t list
  | Abstr of Id.t list * t
  | Let of Id.t * t * t
  | LetTuple of Id.t list * t * t
  | LetRec of Id.t * t * t

let rec fv (e:t): Id.t list =
  match e with
    | Int(_)
    | Char(_)
    | Float(_)
    | String(_) -> []
    | Ident(id) -> [id]
    | If(e0, e1, e2) -> (fv e0) @ (fv e1) @ (fv e2)
    | Tuple(el) -> fvl el
    | Sequence(e1, e2) -> (fv e1) @ (fv e2)
    | App(e, el) -> (fv e) @ (fvl el)
    | Abstr(idl, e) -> List.filter (fun id -> not (List.mem id idl)) (fv e)
    | Let(id, e1, e2) -> (fv e1) @ (Listutils.remove id (fv e2))
    | LetTuple(idl, e1, e2) -> (fv e1) @ (List.filter (fun id -> not (List.mem id idl)) (fv e2))
    | LetRec(id, e1, e2) -> Listutils.remove id ((fv e1) @ (fv e2))
and fvl (el:t list): Id.t list =
  List.fold_left (fun idl e -> List.rev_append (fv e) idl) [] el

let rec subst (id':Id.t) (e':t) (e:t): t =
  let rename (idl:Id.t list) (e:t): Id.t list * t =
    let fv_e' = fv e' in
    let rec rename_aux idl e =
      match idl with
        | [] -> [], e
        | id :: idl ->
            let idl, e = rename_aux idl e in
              if List.mem id idl then
                id :: idl, e
              else
                let id' = Id.generate id ((Listutils.remove id (fv e)) @ (fv_e')) in
                  id' :: idl, subst id (Ident(id')) e
    in rename_aux idl e
  in match e with
    | Int(_)
    | Char(_)
    | Float(_)
    | String(_) as e ->
        e
    | Ident(id) when id = id' ->
        e'
    | Ident(_) as e ->
        e
    | If(e0, e1, e2) ->
        If(subst id' e' e0, subst id' e' e1, subst id' e' e2)
    | Tuple(el) ->
        Tuple(substl id' e' el)
    | Sequence(e1, e2) ->
        Sequence(subst id' e' e1, subst id' e' e2)
    | App(e, el) ->
        App(subst id' e' e, substl id' e' el)
    | Abstr(idl, _) as e when List.mem id' idl ->
        e
    | Abstr(idl, e) ->
        let idl, e = rename idl e in
          Abstr(idl, subst id' e' e)
    | Let(id, e1, e2) when id = id' ->
        Let(id, subst id' e' e1, e2)
    | Let(id, e1, e2) ->
        (match rename [id] e2 with
           | [id], e2 -> Let(id, subst id' e' e1, subst id' e' e2)
           | _ -> assert false)
    | LetTuple(idl, e1, e2) when List.mem id' idl ->
        LetTuple(idl, subst id' e' e1, e2)
    | LetTuple(idl, e1, e2) ->
        let idl, e2 = rename idl e2 in
          LetTuple(idl, subst id' e' e1, subst id' e' e2)
    | LetRec(id, _, _) as e when id = id' ->
        e
    | LetRec(id, e1, e2) ->
        let id'' = Id.generate id ((fv e') @ (Listutils.remove id ((fv e1) @ (fv e2)))) in
        let e1'' = subst id' e' (subst id (Ident(id'')) e1) in
        let e2'' = subst id' e' (subst id (Ident(id'')) e2) in
          LetRec(id'', e1'', e2'')
and substl (id':Id.t) (e':t) (el:t list): t list =
  List.map (subst id' e') el

let rec letify (idl:Id.t list) (el:t list) (e':t): t =
  match idl, el with
    | [], [] ->
        e'
    | id :: idl, e :: el ->
        let e' = letify idl el e' in
        let id' = Id.generate id ((fvl el) @ (Listutils.remove id (fv e'))) in
          Let(id', e, subst id (Ident(id')) e')
    | _, _ -> raise (Invalid_argument("Syntax.letify"))

let rec to_string (e:t): string =
  match e with
    | Int(i) ->
        string_of_int i
    | Char(c) ->
        String.make 1 c
    | Float(f) ->
        string_of_float f
    | String(s) ->
        "\"" ^ s ^ "\""
    | Ident(id) ->
        Id.to_string id
    | If(e0, e1, e2) ->
        "(if " ^ (to_string e0) ^ " then " ^ (to_string e1) ^ " else " ^ (to_string e2) ^ ")"
    | Tuple(el) ->
        "(" ^ (Listutils.to_string ", " to_string el) ^ ")"
    | Sequence(e1, e2) ->
        "(" ^ (to_string e1) ^ "; " ^ (to_string e2) ^ ")"
    | App(e, el) ->
        "(" ^ (to_string e) ^ " " ^ (Listutils.to_string " " to_string el) ^ ")"
    | Abstr(idl, e) ->
        "(lambda " ^ (Listutils.to_string ", " Id.to_string idl) ^ ". " ^ (to_string e) ^ ")"
    | Let(id, e1, e2) ->
        "(let " ^ (Id.to_string id) ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
    | LetTuple(idl, e1, e2) ->
        "(let " ^ (Listutils.to_string ", " Id.to_string idl) ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
    | LetRec(id, e1, e2) ->
        "(let rec " ^ (Id.to_string id) ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
