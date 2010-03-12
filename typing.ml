open Syntax

exception Error of Syntax.t * Type.t * Type.t
exception Unify_error of Type.t * Type.t
exception Unknown_identifier of string

let rec occur (alpha:Type.variable) (tau:Type.t): bool =
  match tau with
    | Type.Arrow(tau, tau') -> occur alpha tau || occur alpha tau'
    | Type.Tuple(taul)
    | Type.Constr(taul, _) -> List.exists (occur alpha) taul
    | Type.Variable(alpha') when alpha == alpha' -> true
    | Type.Variable({ contents = Some(tau) }) -> occur alpha tau
    | Type.Variable({ contents = None }) -> false
    | Type.Poly(_) -> raise (Invalid_argument("Typing.occur"))

let rec unify (tau1:Type.t) (tau2:Type.t): unit =
  try 
    match tau1, tau2 with
      | Type.Arrow(tau1, tau1'), Type.Arrow(tau2, tau2') ->
          unify tau1 tau2;
          unify tau1' tau2'
      | Type.Tuple(tau1l), Type.Tuple(tau2l) ->
          List.iter2 unify tau1l tau2l
      | Type.Constr(tau1l, name1), Type.Constr(tau2l, name2) when name1 = name2 ->
          List.iter2 unify tau1l tau2l
      | Type.Poly(_), _
      | _, Type.Poly(_) ->
          raise (Invalid_argument("Typing.unify"))
      | Type.Variable(alpha1), Type.Variable(alpha2) when alpha1 == alpha2 ->
          ()
      | Type.Variable({ contents = Some(tau1) }), tau2
      | tau1, Type.Variable({ contents = Some(tau2) }) ->
          unify tau1 tau2
      | Type.Variable({ contents = None } as alpha), tau
      | tau, Type.Variable({ contents = None } as alpha) when not (occur alpha tau) ->
          alpha := Some(tau)
      | tau1, tau2 -> raise (Unify_error(tau1, tau2))
  with
    | Invalid_argument("List.iter2") -> raise (Unify_error(tau1, tau2))

let rec is_nonexpansive (e:t): bool =
  match e.desc with
    | Int(_)
    | Char(_)
    | Float(_)
    | String(_)
    | Ident(_)
    | Abstr(_) -> true
    | Tuple(el) -> List.for_all is_nonexpansive el
    | Sequence(_, e2) -> is_nonexpansive e2
    | App(_) -> false
    | If(_, e1, e2) 
    | Let(_, e1, e2)
    | LetTuple(_, e1, e2)
    | LetRec(_, e1, e2) -> is_nonexpansive e1 && is_nonexpansive e2

let generalize (gamma:Type.environment) (e:t) (tau:Type.t): Type.t =
  if is_nonexpansive e
  then Type.generalize gamma tau
  else tau

let rec infer (gamma:Type.environment) (e:t): Type.t =
  let tau = match e.desc with
    | Int(_) ->
        Type.tint
    | Char(_) ->
        Type.tchar
    | Float(_) ->
        Type.tfloat
    | String(_) ->
        Type.tstring
    | Ident(id) ->
        (try
           Type.instantiate (List.assoc id gamma)
         with
           | Not_found -> raise (Unknown_identifier(id)))
    | If(e0, e1, e2) ->
        let tau = Type.newvar () in
          solve gamma e0 Type.tbool;
          solve gamma e1 tau;
          solve gamma e2 tau;
          tau
    | Tuple(el) ->
        Type.Tuple(List.map (fun e -> let tau = Type.newvar () in solve gamma e tau; tau) el)
    | Sequence(e1, e2) ->
        solve gamma e1 Type.tunit;
        infer gamma e2
    | App(e, el) ->
        let tau' = Type.newvar () in
        let tau = List.fold_right (fun e tau -> Type.Arrow(infer gamma e, tau)) el tau' in
          solve gamma e tau;
          tau'
    | Abstr(id, e) ->
        let tau = Type.newvar () in
        let tau' = Type.newvar () in
          solve ((id, tau) :: gamma) e tau';
          Type.Arrow(tau, tau')
    | Let(id, e1, e2) ->
        let tau = infer gamma e1 in
          infer ((id, generalize gamma e1 tau) :: gamma) e2
    | LetTuple(idl, e1, e2) ->
        let taul = List.map (fun _ -> Type.newvar ()) idl in
          solve gamma e1 (Type.Tuple(taul));
          let taul = List.map (generalize gamma e1) taul in
            infer (List.rev_append (List.combine idl taul) gamma) e2
    | LetRec(id, e1, e2) ->
        let tau = Type.newvar () in
          solve ((id, tau) :: gamma) e1 tau;
          infer ((id, generalize gamma e1 tau) :: gamma) e2
  in let tau = Type.normalize tau in
    e.gamma <- gamma;
    e.tau <- tau;
    tau
and solve (gamma:Type.environment) (e:t) (tau:Type.t) =
  try
    unify (infer gamma e) tau
  with
    | Unify_error(tau1, tau2) -> raise (Error(e, Type.normalize tau1, Type.normalize tau2))

