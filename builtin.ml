open Type

type t = Id.t * Type.t * Purity.t

let builtins: t list =
  let alpha = newvar () in
  let beta = newvar () in
  let alpha_alpha_to_bool = Arrow([alpha; alpha], tbool) in
  let int_to_int = Arrow([tint], tint) in
  let int_int_to_int = Arrow([tint; tint], tint) in
  let float_float_to_float = Arrow([tfloat; tfloat], tfloat) in
  let alpha_times_beta = Tuple([alpha; beta]) in
  let alpha_ref = tref alpha in
  let int_ref = tref tint in
  let pure = Purity.Pure in
  let impure1 = Purity.Arrow(Purity.Impure) in
  let pure1 = Purity.Arrow(impure1) in
  let builtins =
    [
      "()",     tunit,                              pure;
      "true",   tbool,                              pure;
      "false",  tbool,                              pure;
      "=",      alpha_alpha_to_bool,                impure1; (* Comparison of closures can raise exception, *)
      "<>",     alpha_alpha_to_bool,                impure1; (* so all of the comparison operators are *)
      "<",      alpha_alpha_to_bool,                impure1; (* considered impure when applied *)
      ">",      alpha_alpha_to_bool,                impure1;
      "<=",     alpha_alpha_to_bool,                impure1;
      ">=",     alpha_alpha_to_bool,                impure1;
      "==",     alpha_alpha_to_bool,                impure1;
      "!=",     alpha_alpha_to_bool,                impure1;
      "~-",     int_to_int,                         pure;
      "+",      int_int_to_int,                     pure;
      "-",      int_int_to_int,                     pure;
      "*",      int_int_to_int,                     pure;
      "land",   int_int_to_int,                     pure;
      "lor",    int_int_to_int,                     pure;
      "lxor",   int_int_to_int,                     pure;
      "lnot",   int_to_int,                         pure;
      "lsl",    int_int_to_int,                     pure;
      "lsr",    int_int_to_int,                     pure;
      "asr",    int_int_to_int,                     pure;
      "~-.",    Arrow([tfloat], tfloat),            pure;
      "+.",     float_float_to_float,               pure;
      "-.",     float_float_to_float,               pure;
      "*.",     float_float_to_float,               pure;
      "^",      Arrow([tstring; tstring], tstring), pure;
      "fst",    Arrow([alpha_times_beta], alpha),   pure1;
      "snd",    Arrow([alpha_times_beta], beta),    pure1;
      "ref",    Arrow([alpha], alpha_ref),          impure1;
      "!",      Arrow([alpha_ref], alpha),          pure1;
      ":=",     Arrow([alpha_ref; alpha], tunit),   impure1;
      "incr",   Arrow([int_ref], tunit),            impure1;
      "decr",   Arrow([int_ref], tunit),            impure1;
      "ignore", Arrow([alpha], tunit),              pure
    ]
  in List.map (fun (id, tau, purity) -> id, Type.generalize [] tau, purity) builtins

let gamma0 = List.map (fun (id, tau, _) -> (id, tau)) builtins
let sigma0 = List.map (fun (id, _, purity) -> (id, purity)) builtins
