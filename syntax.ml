type t =
    { mutable gamma: Type.environment;
      desc: desc;
      mutable tau: Type.t }
and desc =
  | Int of int
  | Char of char
  | Float of float
  | String of string
  | Ident of string
  | If of t * t * t
  | Tuple of t list
  | Sequence of t * t
  | App of t * t
  | Abstr of string * t
  | Let of string * t * t
  | LetTuple of string list * t * t
  | LetRec of string * t * t

let rec to_string (e:t): string =
  match e.desc with
    | Int(i) ->
        string_of_int i
    | Char(c) ->
        String.make 1 c
    | Float(f) ->
        string_of_float f
    | String(s) ->
        "\"" ^ s ^ "\""
    | Ident(id) ->
        id
    | If(e0, e1, e2) ->
        "(if" ^ (to_string e0) ^ " then " ^ (to_string e1) ^ " else " ^ (to_string e2) ^ ")"
    | Tuple(el) ->
        "(" ^ (List.fold_left
                 (fun s e ->
                    (if s = "" then "" else s ^ ", ") ^ (to_string e))
                 ""
                 el) ^ ")"
    | Sequence(e1, e2) ->
        "(" ^ (to_string e1) ^ "; " ^ (to_string e2) ^ ")"
    | App(e1, e2) ->
        "(" ^ (to_string e1) ^ " " ^ (to_string e2) ^ ")"
    | Abstr(id, e) ->
        "(lambda " ^ id ^ ". " ^ (to_string e) ^ ")"
    | Let(id, e1, e2) ->
        "(let " ^ id ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
    | LetTuple(idl, e1, e2) ->
        "(let " ^ (List.fold_left
                     (fun s id ->
                        if s = "" then id else s ^ ", " ^ id)
                     ""
                     idl) ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
    | LetRec(id, e1, e2) ->
        "(let rec " ^ id ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
