type environment = 
    (string * Type.t) list

exception Error of Syntax.t * Type.t * Type.t
exception Unify_error of Type.t * Type.t
exception Unknown_identifier of string

let rec occur (alpha:Type.variable) (tau:Type.t): bool =
  match tau with
    | Type.Arrow(tau, tau') -> occur alpha tau || occur alpha tau'
    | Type.Tuple(taul) -> List.exists (occur alpha) taul
    | Type.Variable(alpha') when alpha == alpha' -> true
    | Type.Variable({ contents = Some(tau) }) -> occur alpha tau
    | Type.Poly(_) -> raise (Invalid_argument("Typing.occur"))
    | _ -> false

let rec unify (tau1:Type.t) (tau2:Type.t): unit =
  match tau1, tau2 with
    | Type.Unit,   Type.Unit
    | Type.Bool,   Type.Bool
    | Type.Int,    Type.Int
    | Type.Char,   Type.Char
    | Type.Float,  Type.Float
    | Type.String, Type.String ->
        ()
    | Type.Arrow(tau1, tau1'), Type.Arrow(tau2, tau2') ->
        unify tau1 tau2;
        unify tau1' tau2'
    | (Type.Tuple(tau1l) as tau1), (Type.Tuple(tau2l) as tau2) ->
        (try
           List.iter2 unify tau1l tau2l
         with
           | Invalid_argument("List.iter2") -> raise (Unify_error(tau1, tau2)))
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

let rec is_nonexpansive (e:Syntax.t): bool =
  match e with
    | Syntax.Unit
    | Syntax.Bool(_)
    | Syntax.Int(_)
    | Syntax.Char(_)
    | Syntax.Float(_)
    | Syntax.String(_)
    | Syntax.Ident(_)
    | Syntax.Abstr(_) -> true
    | Syntax.Tuple(el) -> List.for_all is_nonexpansive el
    | Syntax.App(_) -> false
    | Syntax.If(_, e1, e2) 
    | Syntax.Let(_, e1, e2)
    | Syntax.LetTuple(_, e1, e2)
    | Syntax.LetRec(_, e1, e2) -> is_nonexpansive e1 && is_nonexpansive e2

let generalize (gamma:environment) (tau:Type.t): Type.t =
  let bvl = List.fold_left (fun bvl (_, tau) -> List.rev_append (Type.fv tau) bvl) [] gamma in
  let fvl = List.filter (fun alpha -> not (List.memq alpha bvl)) (Listutils.normalizeq (Type.fv tau)) in
    List.fold_left (fun tau alpha -> Type.Poly(alpha, tau)) tau fvl

let rec instantiate (tau:Type.t): Type.t =
  match tau with
    | Type.Poly(alpha, tau) -> Type.fresh (instantiate tau) alpha
    | tau -> tau

let rec typeof (gamma:environment) (e:Syntax.t): Type.t =
  let tau = match e with
    | Syntax.Unit ->
        Type.Unit
    | Syntax.Bool(_) ->
        Type.Bool
    | Syntax.Int(_) ->
        Type.Int
    | Syntax.Char(_) ->
        Type.Char
    | Syntax.Float(_) ->
        Type.Float
    | Syntax.String(_) ->
        Type.String
    | Syntax.Ident(id) ->
        (try
           instantiate (List.assoc id gamma)
         with
           | Not_found -> raise (Unknown_identifier(id)))
    | Syntax.If(e0, e1, e2) ->
        let tau = Type.newvar () in
          solve gamma e0 Type.Bool;
          solve gamma e1 tau;
          solve gamma e2 tau;
          tau
    | Syntax.Tuple(el) ->
        Type.Tuple(List.map (fun e -> let tau = Type.newvar () in solve gamma e tau; tau) el)
    | Syntax.App(e1, e2) ->
        let tau = Type.newvar () in
        let tau' = Type.newvar () in
          solve gamma e1 (Type.Arrow(tau, tau'));
          solve gamma e2 tau;
          tau'
    | Syntax.Abstr(id, e) ->
        let tau = Type.newvar () in
        let tau' = Type.newvar () in
          solve ((id, tau) :: gamma) e tau';
          Type.Arrow(tau, tau')
    | Syntax.Let(id, e1, e2) ->
        let tau = typeof gamma e1 in
        let tau = if is_nonexpansive e1 then generalize gamma tau else tau in
          typeof ((id, tau) :: gamma) e2
    | Syntax.LetTuple(idl, e1, e2) ->
        let taul = List.map (fun _ -> Type.newvar ()) idl in
          solve gamma e1 (Type.Tuple(taul));
          let taul = if is_nonexpansive e1 then List.map (generalize gamma) taul else taul in
            typeof (List.rev_append (List.combine idl taul) gamma) e2
    | Syntax.LetRec(id, e1, e2) ->
        let tau = Type.newvar () in
          solve ((id, tau) :: gamma) e1 tau;
          let tau = if is_nonexpansive e1 then generalize gamma tau else tau in
            typeof ((id, tau) :: gamma) e2
  in Type.normalize tau
and solve (gamma:environment) (e:Syntax.t) (tau:Type.t) =
  try
    unify (typeof gamma e) tau
  with
    | Unify_error(tau1, tau2) -> raise (Error(e, Type.normalize tau1, Type.normalize tau2))

