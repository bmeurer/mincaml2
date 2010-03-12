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
  | App of t * t list
  | Abstr of string list * t
  | Let of string * t * t
  | LetTuple of string list * t * t
  | LetRec of string * t * t

let rec to_string (e:t): string =
  let list_to_string sep to_string xl: string =
    List.fold_left (fun s x -> let sx = to_string x in if s = "" then sx else s ^ sep ^ sx) "" xl
  in match e.desc with
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
        "(" ^ (list_to_string ", " to_string el) ^ ")"
    | Sequence(e1, e2) ->
        "(" ^ (to_string e1) ^ "; " ^ (to_string e2) ^ ")"
    | App(e, el) ->
        "(" ^ (to_string e) ^ " " ^ (list_to_string " " to_string el) ^ ")"
    | Abstr(idl, e) ->
        "(lambda " ^ (list_to_string ", " (fun x -> x) idl) ^ ". " ^ (to_string e) ^ ")"
    | Let(id, e1, e2) ->
        "(let " ^ id ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
    | LetTuple(idl, e1, e2) ->
        "(let " ^ (list_to_string ", " (fun x -> x) idl) ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
    | LetRec(id, e1, e2) ->
        "(let rec " ^ id ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2) ^ ")"
