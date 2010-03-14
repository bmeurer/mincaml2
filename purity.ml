type t =
  | Pure            (* always pure, no matter how often this is applied *)
  | Impure          (* may have side effects *)
  | Arrow of t      (* pure itself and given purity when applied *)
  | Tuple of t list (* tuple expression with given purities *)

type environment =
    (Id.t * t) list

let rec sup (p1:t) (p2:t): t =
  match p1, p2 with
    | Pure, Pure ->
        Pure
    | Impure, _
    | _, Impure ->
        Impure
    | (Pure as p1), Arrow(p2)
    | Arrow(p1), (Pure as p2)
    | Arrow(p1), Arrow(p2) ->
        Arrow(sup p1 p2)
    | Pure, Tuple(pl)
    | Tuple(pl), Pure ->
        Tuple(List.map (sup Pure) pl)
    | Tuple(p1l), Tuple(p2l) ->
        Tuple(List.map2 sup p1l p2l)
    | Arrow(_), Tuple(_)
    | Tuple(_), Arrow(_) ->
        Arrow(Impure)

let rec check (sigma:environment) (e:Syntax.t): t =
  match e with
    | Syntax.Int(_)
    | Syntax.Char(_)
    | Syntax.Float(_)
    | Syntax.String(_) ->
        Pure
    | Syntax.Ident(id) ->
        List.assoc id sigma
    | Syntax.If(e0, e1, e2) ->
        (match check sigma e0, check sigma e1, check sigma e2 with
           | Impure, _, _ -> Impure
           | _, p1, p2 -> sup p1 p2)
    | Syntax.Tuple(el) ->
        (match List.map (check sigma) el with
           | pl when List.exists ((=) Impure) pl ->
               Impure
           | pl -> Tuple(pl))
    | Syntax.Sequence(e1, e2) ->
        (match check sigma e1, check sigma e2 with
           | Impure, _ -> Impure
           | _, p2 -> p2)
    | Syntax.App(e, el) ->
        (match check sigma e, List.map (check sigma) el with
           | (Pure as p), pl
           | Arrow(p), pl when not (List.exists ((=) Impure) pl) -> p
           | _ -> Impure)
    | Syntax.Abstr(idl, e) ->
        let sigma = List.fold_right (fun id sigma -> (id, Arrow(Impure)) :: sigma) idl sigma in
          Arrow(check sigma e)
    | Syntax.Let(id, e1, e2) ->
        check ((id, check sigma e1) :: sigma) e2
    | Syntax.LetTuple(idl, e1, e2) ->
        (match check sigma e1 with
           | Tuple(pl) -> check (List.rev_append (List.combine idl pl) sigma) e2
           | Pure -> check (List.fold_right (fun id sigma -> (id, Pure) :: sigma) idl sigma) e2
           | _ -> check (List.fold_right (fun id sigma -> (id, Impure) :: sigma) idl sigma) e2)
    | Syntax.LetRec(id, e1, e2) ->
        check ((id, check ((id, Impure) :: sigma) e1) :: sigma) e2
