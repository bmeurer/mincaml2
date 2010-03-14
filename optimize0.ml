let rec optimize (sigma:Purity.environment) (e:Syntax.t): Syntax.t =
  match e with
    | Syntax.Int(_)
    | Syntax.Char(_)
    | Syntax.Float(_)
    | Syntax.String(_)
    | Syntax.Ident(_) as e ->
        e
    | Syntax.If(e0, e1, e2) ->
        optimize_if sigma (optimize sigma e0) (optimize sigma e1) (optimize sigma e2)
    | Syntax.Tuple(el) ->
        Syntax.Tuple(List.map (optimize sigma) el)
    | Syntax.Sequence(e1, e2) ->
        (match optimize sigma e1, optimize sigma e2 with
           | e1, e2 when Purity.check sigma e1 <> Purity.Impure ->
               e2
           | e1, e2 ->
               Syntax.Sequence(e1, e2))
    | Syntax.App(e, el) ->
        optimize_app sigma (optimize sigma e) (List.map (optimize sigma) el)
    | Syntax.Abstr(idl, e) ->
        Syntax.Abstr(idl, optimize sigma e)
    | Syntax.Let(id, e1, e2) ->
        (match optimize sigma e1, optimize ((id, Purity.check sigma e1) :: sigma) e2 with
           | (Syntax.Int(_) as e1), e2
           | (Syntax.Char(_) as e1), e2
           | (Syntax.Float(_) as e1), e2
           | (Syntax.String(_) as e1), e2
           | (Syntax.Ident(_) as e1), e2 ->
               optimize sigma (Syntax.subst id e1 e2)
           | Syntax.Sequence(e1', e1), e2 ->
               optimize sigma (Syntax.Sequence(e1', Syntax.Let(id, e1, e2)))
           | e1, Syntax.Sequence(e2, e2') when not (List.mem id (Syntax.fv e2')) ->
               optimize sigma (Syntax.Sequence(Syntax.Let(id, e1, e2), e2'))
           | Syntax.Let(id1, e1, e2), e ->
               let id1' = Id.generate id ((Syntax.fv e) @ (Listutils.remove id1 (Syntax.fv e2))) in
                 optimize sigma (Syntax.Let(id1',
                                            e1,
                                            Syntax.Let(id,
                                                       Syntax.subst id1 (Syntax.Ident(id1')) e2,
                                                       e)))
           | e1, e2 when Purity.check sigma e1 <> Purity.Impure && (Listutils.count id (Syntax.fv e2)) <= 1 ->
               optimize sigma (Syntax.subst id e1 e2)
           | e1, e2 when not (List.mem id (Syntax.fv e2)) ->
               optimize sigma (Syntax.Sequence(Syntax.App(Syntax.Ident("ignore"), [e1]), e2))
           | e1, e2 ->
               Syntax.Let(id, e1, e2))
    | Syntax.LetTuple(idl, e1, e2) ->
        let sigma' =
          (match Purity.check sigma e1 with
             | Purity.Tuple(pl) -> List.rev_append (List.combine idl pl) sigma
             | Purity.Pure -> List.fold_right (fun id sigma -> (id, Purity.Pure) :: sigma) idl sigma
             | _ -> List.fold_right (fun id sigma -> (id, Purity.Impure) :: sigma) idl sigma)
        in (match optimize sigma e1, optimize sigma' e2 with
              | Syntax.Tuple(el), e ->
                  optimize sigma (Syntax.letify idl el e)
              | Syntax.Sequence(e1', e1), e2 ->
                  optimize sigma (Syntax.Sequence(e1', Syntax.LetTuple(idl, e1, e2)))
              | e1, e2 ->
                  Syntax.LetTuple(idl, e1, e2))
    | Syntax.LetRec(id, e1, e2) ->
        let sigma1 = (id, Purity.Impure) :: sigma in
        let p1 = Purity.check sigma1 e1 in
        let sigma2 = (id, p1) :: sigma in
          (match optimize sigma1 e1, optimize sigma2 e2 with
             | e1, e2 when not (List.mem id (Syntax.fv e2)) && p1 <> Purity.Impure ->
                 e2
             | e1, e2 when not (List.mem id (Syntax.fv e1)) ->
                 optimize sigma (Syntax.Let(id, e1, e2))
             | e1, e2 ->
                 Syntax.LetRec(id, e1, e2))
and optimize_if sigma e0 e1 e2 =
  let negate (op:string): string =
    match op with
      | "="  -> "<>"
      | "<>" -> "="
      | "<"  -> ">="
      | ">"  -> "<="
      | "<=" -> ">"
      | ">=" -> "<"
      | "==" -> "!="
      | "!=" -> "=="
      | _    -> ""
  in (match e0, e1, e2 with
        | Syntax.Ident("true"), e1, _ ->
            e1
        | Syntax.Ident("false"), _, e2 ->
            e2
        | e0, e1, e2 when e1 = e2 ->
            optimize sigma (Syntax.Sequence(Syntax.App(Syntax.Ident("ignore"), [e0]),
                                            Syntax.Sequence(Syntax.App(Syntax.Ident("ignore"), [e1]),
                                                            e2)))
        | e0, Syntax.Ident("true"), Syntax.Ident("false") ->
            e0
        | Syntax.If(e0, Syntax.Ident("false"), Syntax.Ident("true")), e1, e2 ->
            Syntax.If(e0, e2, e1)
        | Syntax.Sequence(e0', e0), e1, e2 ->
            optimize sigma (Syntax.Sequence(e0', Syntax.If(e0, e1, e2)))
        | Syntax.App(Syntax.Ident(op), el), e1, (Syntax.Ident("true") as e2) 
        | Syntax.App(Syntax.Ident(op), el), (Syntax.Ident("false") as e1), e2 when negate op <> "" ->
            optimize sigma (Syntax.If(Syntax.App(Syntax.Ident(negate op), el), e2, e1))
        | Syntax.Let(id, e1', e2'), e1, e2 ->
            let id' = Id.generate id ((Listutils.remove id (Syntax.fv e2')) @ (Syntax.fv e1) @ (Syntax.fv e2)) in
              optimize sigma (Syntax.Let(id',
                                         e1',
                                         Syntax.If(Syntax.subst id (Syntax.Ident(id')) e2',
                                                   e1, e2)))
                (* TODO - same for LetRec and LetTuple *)
        | e0, e1, e2 ->
            Syntax.If(e0, e1, e2))
and optimize_app sigma e el =
  match e, el with
    (* Comparing syntactically equal expressions (TODO - comparing closure raises an exception) *)
    | Syntax.Ident("="), [e1; e2]
    | Syntax.Ident("=="), [e1; e2]
    | Syntax.Ident("<="), [e1; e2]
    | Syntax.Ident(">="), [e1; e2] when e1 = e2 ->
        optimize sigma (Syntax.Sequence(Syntax.App(Syntax.Ident("ignore"), [e1]),
                                        Syntax.Sequence(Syntax.App(Syntax.Ident("ignore"), [e2]),
                                                        Syntax.Ident("true"))))
    | Syntax.Ident("<>"), [e1; e2]
    | Syntax.Ident("!="), [e1; e2]
    | Syntax.Ident("<"), [e1; e2]
    | Syntax.Ident(">"), [e1; e2] when e1 = e2 ->
        optimize sigma (Syntax.Sequence(Syntax.App(Syntax.Ident("ignore"), [e1]),
                                        Syntax.Sequence(Syntax.App(Syntax.Ident("ignore"), [e2]),
                                                        Syntax.Ident("false"))))
    (* Comparing with () *)
    | Syntax.Ident("="), [Syntax.Ident("()"); e]
    | Syntax.Ident("="), [e; Syntax.Ident("()")]
    | Syntax.Ident("=="), [Syntax.Ident("()"); e]
    | Syntax.Ident("=="), [e; Syntax.Ident("()")]
    | Syntax.Ident("<="), [Syntax.Ident("()"); e]
    | Syntax.Ident("<="), [e; Syntax.Ident("()")]
    | Syntax.Ident(">="), [Syntax.Ident("()"); e]
    | Syntax.Ident(">="), [e; Syntax.Ident("()")] ->
        optimize sigma (Syntax.Sequence(e, Syntax.Ident("true")))
    | Syntax.Ident("<>"), [Syntax.Ident("()"); e]
    | Syntax.Ident("<>"), [e; Syntax.Ident("()")]
    | Syntax.Ident("!="), [Syntax.Ident("()"); e]
    | Syntax.Ident("!="), [e; Syntax.Ident("()")]
    | Syntax.Ident("<"), [Syntax.Ident("()"); e]
    | Syntax.Ident("<"), [e; Syntax.Ident("()")]
    | Syntax.Ident(">"), [Syntax.Ident("()"); e]
    | Syntax.Ident(">"), [e; Syntax.Ident("()")] ->
        optimize sigma (Syntax.Sequence(e, Syntax.Ident("false")))
    (* Comparing booleans *)
    | Syntax.Ident("="), [Syntax.Ident("true"); e]
    | Syntax.Ident("="), [e; Syntax.Ident("true")]
    | Syntax.Ident("=="), [Syntax.Ident("true"); e]
    | Syntax.Ident("=="), [e; Syntax.Ident("true")]
    | Syntax.Ident("<>"), [Syntax.Ident("false"); e]
    | Syntax.Ident("<>"), [e; Syntax.Ident("false")]
    | Syntax.Ident("!="), [Syntax.Ident("false"); e]
    | Syntax.Ident("!="), [e; Syntax.Ident("false")] ->
        e
    | Syntax.Ident("="), [Syntax.Ident("false"); e]
    | Syntax.Ident("="), [e; Syntax.Ident("false")]
    | Syntax.Ident("=="), [Syntax.Ident("false"); e]
    | Syntax.Ident("=="), [e; Syntax.Ident("false")]
    | Syntax.Ident("<>"), [Syntax.Ident("true"); e]
    | Syntax.Ident("<>"), [e; Syntax.Ident("true")]
    | Syntax.Ident("!="), [Syntax.Ident("true"); e]
    | Syntax.Ident("!="), [e; Syntax.Ident("true")] ->
        optimize sigma (Syntax.If(e, Syntax.Ident("false"), Syntax.Ident("true")))
    | Syntax.Ident("<"), [Syntax.Ident("true"); e]
    | Syntax.Ident("<"), [e; Syntax.Ident("false")]
    | Syntax.Ident(">"), [e; Syntax.Ident("true")]
    | Syntax.Ident(">"), [Syntax.Ident("false"); e] ->
        optimize sigma (Syntax.Sequence(e, Syntax.Ident("false")))
    | Syntax.Ident("<="), [e; Syntax.Ident("true")]
    | Syntax.Ident("<="), [Syntax.Ident("false"); e]
    | Syntax.Ident(">="), [Syntax.Ident("true"); e]
    | Syntax.Ident(">="), [e; Syntax.Ident("false")] ->
        optimize sigma (Syntax.Sequence(e, Syntax.Ident("true")))
    (* Comparing ints, chars, floats, strings *)
    | Syntax.Ident(op), [(Syntax.Int(_) as e1); (Syntax.Int(_) as e2)]
    | Syntax.Ident(op), [(Syntax.Char(_) as e1); (Syntax.Char(_) as e2)]
    | Syntax.Ident(op), [(Syntax.Float(_) as e1); (Syntax.Float(_) as e2)]
    | Syntax.Ident(op), [(Syntax.String(_) as e1); (Syntax.String(_) as e2)] when op = "=" || op = "<>" || op = "<" || op = ">" || op = "<=" || op = ">=" || op = "==" || op = "!=" ->
        if (match op with
           | "="
           | "==" -> e1 = e2
           | "<>"
           | "!=" -> e1 <> e2
           | "<"  -> e1 < e2
           | ">"  -> e1 > e2
           | "<=" -> e1 <= e2
           | ">=" -> e1 >= e2
           | _    -> assert false)
        then Syntax.Ident("true")
        else Syntax.Ident("false")
    (* Arithmetics on ints *)
    | Syntax.Ident("~-"), [Syntax.Int(i)] ->
        Syntax.Int(- i)
    | Syntax.Ident("-"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 - i2)
    | Syntax.Ident("*"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 * i2)
    | Syntax.Ident("land"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 land i2)
    | Syntax.Ident("lor"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 lor i2)
    | Syntax.Ident("lxor"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 lxor i2)
    | Syntax.Ident("lnot"), [Syntax.Int(i)] ->
        Syntax.Int(lnot i)
    | Syntax.Ident("lsl"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 lsl i2)
    | Syntax.Ident("lsr"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 lsr i2)
    | Syntax.Ident("asr"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 asr i2)
    (* Arithmetics on floats *)
    | Syntax.Ident("~-."), [Syntax.Float(f)] ->
        Syntax.Float(-. f)
    | Syntax.Ident("+."), [Syntax.Float(f1); Syntax.Float(f2)] ->
        Syntax.Float(f1 +. f2)
    | Syntax.Ident("-."), [Syntax.Float(f1); Syntax.Float(f2)] ->
        Syntax.Float(f1 -. f2)
    | Syntax.Ident("*."), [Syntax.Float(f1); Syntax.Float(f2)] ->
        Syntax.Float(f1 *. f2)
    (* Arithmetic optimizations *)
    | Syntax.Ident(op), [Syntax.App(Syntax.Ident(op'), [e])] when op = op' && (op = "~-" || op = "~-.") ->
        e
(* TODO
    | Syntax.Ident("~-"), [Syntax.App(Syntax.Ident("+"), [e1; e2])] ->
        optimize (Syntax.LetTuple(["t1"; "t2"],
                                 Syntax.Tuple([e1;
                                               Syntax.App(Syntax.Ident("~-"), [e2])]),
                                 Syntax.App(Syntax.Ident("-"), [Syntax.Ident("t2"); Syntax.Ident("t1")])))
    | Syntax.Ident("~-"), [Syntax.App(Syntax.Ident("-"), [e1; e2])] ->
        optimize (Syntax.LetTuple(["t1"; "t2"],
                                 Syntax.Tuple([e1; e2]),
                                 Syntax.App(Syntax.Ident("-"), [Syntax.Ident("t2"); Syntax.Ident("t1")])))
    | Syntax.Ident("+"), [Syntax.Int(i1); Syntax.Int(i2)] ->
        Syntax.Int(i1 + i2)
    | Syntax.Ident("+"), [Syntax.App(Syntax.Ident("~-"), [e1]); e2] ->
    | Syntax.Ident("+"), [e2; Syntax.App(Syntax.Ident("~-"), [e1])] ->
        optimize (Syntax.LetTuple(["t1"; "t2"],
                                 Syntax.Tuple([e1; e2]),
                                 Syntax.App(Syntax.Ident("-"), [Syntax.Ident("t2"); Syntax.Ident("t1")])))
*)
    | Syntax.Ident("^"), [Syntax.String(s1); Syntax.String(s2)] ->
        Syntax.String(s1 ^ s2)
    (* Optimizations for fst *)
    | Syntax.Ident("fst"), [e] ->
        optimize sigma (Syntax.LetTuple(["t1"; "t2"], e, Syntax.Ident("t1")))
    (* Optimizations for snd *)
    | Syntax.Ident("snd"), [e] ->
        optimize sigma (Syntax.LetTuple(["t1"; "t2"], e, Syntax.Ident("t2")))
    (* Optimizations for imperative operators *)
    | Syntax.Ident("!"), [Syntax.App(Syntax.Ident("ref"), [e])] ->
        e
    | Syntax.Ident(":="), [Syntax.App(Syntax.Ident("ref"), [e1]); e2] ->
        optimize sigma (Syntax.LetTuple(["t1"; "t2"], Syntax.Tuple([e1; e2]), Syntax.Ident("()")))
    (* Move applications into if..then..else.. *)
    | Syntax.If(e0, e1, e2), el ->
        let fv = (Syntax.fv e1) @ (Syntax.fv e2) in
        let id = Id.generate "cond" fv in
        let idl = Id.generaten (List.length el) "arg" (id :: ((Syntax.fv e0) @ fv)) in
        let identl = List.map (fun id -> Syntax.Ident(id)) idl in
          optimize sigma (Syntax.Let(id,
                                     e0,
                                     Syntax.letify idl el (Syntax.If(Syntax.Ident(id),
                                                                     Syntax.App(e1, identl),
                                                                     Syntax.App(e2, identl)))))
    (* Move sequencing out of applications *)
    | Syntax.Sequence(e', e), el ->
        optimize sigma (Syntax.Sequence(e', Syntax.App(e, el)))
    (* Letify applications of abstractions *)
    | Syntax.Abstr(idl, e), el ->
        optimize sigma (Syntax.letify idl el e)
    | e, el ->
        Syntax.App(e, el)
