type t =
    { 
      cg_context:          Llvm.llcontext;
      cg_module:           Llvm.llmodule;
      cg_builder:          Llvm.llbuilder;
      cg_fpmanager:        [ `Function ] Llvm.PassManager.t;

      cg_i1_type:          Llvm.lltype;
      cg_int_type:         Llvm.lltype;
      cg_float_type:       Llvm.lltype;
      cg_floatbox_type:    Llvm.lltype;
      cg_value_type:       Llvm.lltype;

      cg_alloc_float_func: Llvm.llvalue;
      cg_alloc_tuple_func: Llvm.llvalue;
      cg_compare_func:     Llvm.llvalue;
    }


(**
 ** Misc stuff
 **)

let const_unit (cg:t): Llvm.llvalue =
  Llvm.undef cg.cg_value_type
let const_int (i:int) (cg:t): Llvm.llvalue =
  Llvm.const_int cg.cg_int_type i
let const_i32 (i:int) (cg:t): Llvm.llvalue =
  Llvm.const_int (Llvm.i32_type cg.cg_context) i
let const_float (f:float) (cg:t): Llvm.llvalue =
  Llvm.const_float cg.cg_float_type f

let tag_float = 0
let tag_tuple = 1
let tag_string = 2


(**
 ** Generic code builders
 **)

let build_box (v:Llvm.llvalue) (name:string) (cg:t): Llvm.llvalue =
  match Llvm.type_of v with
    | vty when vty = cg.cg_value_type ->
        v
    | vty when vty = cg.cg_i1_type ->
        let v_sext = Llvm.build_sext v cg.cg_int_type (name ^ "_sext") cg.cg_builder in
          Llvm.build_inttoptr v_sext cg.cg_value_type name cg.cg_builder
    | vty when vty = cg.cg_int_type ->
        let v_shl = Llvm.build_shl v (const_int 1 cg) (name ^ "_shl") cg.cg_builder in
        let v_or = Llvm.build_or v_shl (const_int 1 cg) (name ^ "_shl_or") cg.cg_builder in
          Llvm.build_inttoptr v_or cg.cg_value_type name cg.cg_builder
    | vty when vty = cg.cg_float_type && Llvm.is_constant v ->
        let boxed = Llvm.const_struct cg.cg_context [|const_int tag_float cg; v|] in
        let global = Llvm.define_qualified_global "mincaml2__const_float" boxed 0 cg.cg_module in
          Llvm.set_global_constant true global;
          Llvm.set_linkage Llvm.Linkage.Private global;
          Llvm.build_bitcast global cg.cg_value_type name cg.cg_builder
    | vty when vty = cg.cg_float_type ->
        let call = Llvm.build_call cg.cg_alloc_float_func [|v|] name cg.cg_builder in
          Llvm.set_instruction_call_conv Llvm.CallConv.fast call;
          Llvm.set_tail_call true call;
          call
    | vty when Llvm.classify_type vty = Llvm.TypeKind.Pointer ->
        Llvm.build_bitcast v cg.cg_value_type name cg.cg_builder
    | vty -> failwith ("Cannot box value of type " ^ (Llvm.string_of_lltype vty))

let build_unbox (v:Llvm.llvalue) (ty:Llvm.lltype) (name:string) (cg:t): Llvm.llvalue =
    match Llvm.type_of v, ty with
      | vty, ty when vty = ty ->
          v
      | vty, ty when vty = cg.cg_value_type && ty = cg.cg_i1_type ->
          Llvm.build_is_not_null v name cg.cg_builder
      | vty, ty when vty = cg.cg_value_type && ty = cg.cg_int_type ->
          let v_or = Llvm.build_ptrtoint v ty (name ^ "_or") cg.cg_builder in
            Llvm.build_ashr v_or (const_int 1 cg) name cg.cg_builder
      | vty, ty when vty = cg.cg_value_type && ty = cg.cg_float_type ->
          let floatbox = Llvm.build_bitcast v (Llvm.pointer_type cg.cg_floatbox_type) "floatbox" cg.cg_builder in
          let floatptr = Llvm.build_struct_gep floatbox 1 "floatptr" cg.cg_builder in
            Llvm.build_load floatptr name cg.cg_builder
      | vty, ty when Llvm.classify_type ty = Llvm.TypeKind.Pointer && vty = cg.cg_value_type ->
          Llvm.build_bitcast v ty name cg.cg_builder
      | vty, ty -> failwith ("Cannot unbox " ^ (Llvm.string_of_lltype vty) ^ " to " ^ (Llvm.string_of_lltype ty))

let build_gep (tuple:Llvm.llvalue) (n:int) (name:string) (cg:t): Llvm.llvalue =
  let ptrtmp = Llvm.build_bitcast tuple (Llvm.pointer_type cg.cg_value_type) "ptrtmp" cg.cg_builder in
    Llvm.build_gep ptrtmp [|const_i32 (n + 1) cg|] name cg.cg_builder

let build_proj (tuple:Llvm.llvalue) (n:int) (name:string) (cg:t): Llvm.llvalue =
  let pointer = build_gep tuple n "pointer" cg in
    Llvm.build_load pointer name cg.cg_builder

let build_tuple (n:int) (name:string) (cg:t): Llvm.llvalue =
  let call = Llvm.build_call cg.cg_alloc_tuple_func [|const_int n cg|] name cg.cg_builder in
    Llvm.set_instruction_call_conv Llvm.CallConv.fast call;
    Llvm.set_tail_call true call;
    call

let build_call (fn:Llvm.llvalue) (args:Llvm.llvalue list) (name:string) (cg:t) =
  let fntype = Llvm.function_type cg.cg_value_type (Array.make (List.length args) cg.cg_value_type) in
  let fntmp = build_unbox fn (Llvm.pointer_type fntype) "fntmp" cg in
  let fncall = Llvm.build_call fntmp (Array.of_list args) name cg.cg_builder in
    Llvm.set_instruction_call_conv Llvm.CallConv.fast fncall;
    Llvm.set_tail_call true fncall;
    fncall

let build_native_function fnname fntype (fncode:Llvm.llvalue -> Llvm.llvalue list -> t -> Llvm.llvalue) (cg:t) =
  let bb = (try Some(Llvm.insertion_block cg.cg_builder) with Not_found -> None) in
  let fn = Llvm.declare_function fnname fntype cg.cg_module in
  let entry = Llvm.append_block cg.cg_context "entry" fn in
    Llvm.position_at_end entry cg.cg_builder;
    let result = fncode fn (Array.to_list (Llvm.params fn)) cg in
      ignore (Llvm.build_ret result cg.cg_builder);
      Llvm_analysis.assert_valid_function fn;
      ignore (Llvm.PassManager.run_function fn cg.cg_fpmanager);
      (match bb with Some(bb) -> Llvm.position_at_end bb cg.cg_builder | None -> ());
      fn

let build_function (name:string) (arity:int) (code:Llvm.llvalue -> Llvm.llvalue list -> t -> Llvm.llvalue) (cg:t) =
  let fnname = (let name = "mincaml2__func_" ^ name in
                let rec generate (n:int): string =
                  match name ^ (string_of_int n) with
                    | name when Llvm.lookup_global name cg.cg_module = None -> name
                    | _ -> generate (n + 1)
                in if Llvm.lookup_global name cg.cg_module = None then name else generate 1) in
  let fncode fn vl cg =
    Llvm.set_function_call_conv Llvm.CallConv.fast fn;
    Llvm.set_linkage Llvm.Linkage.Private fn;
    build_box (code fn vl cg) "result" cg in
  let fntype = Llvm.function_type cg.cg_value_type (Array.make arity cg.cg_value_type) in
  let fn = build_native_function fnname fntype fncode cg in
      fn

let build_trampoline (name:string) (arity:int) (code:Llvm.llvalue list -> t -> Llvm.llvalue) (cg:t): Llvm.llvalue =
  let fnname = "mincaml2__trampoline$" ^ name in
    match Llvm.lookup_global fnname cg.cg_module with
      | Some(fn) -> fn
      | None -> build_function ("trampoline$" ^ name) arity (fun _ vl cg -> code vl cg) cg


(**
 ** Builtins
 **)

let rec builtin_compare (op:Id.t) (vl:Llvm.llvalue list) (cg:t): Llvm.llvalue =
  let op_to_cmp (op:Id.t): Llvm.Icmp.t * Llvm.Fcmp.t =
    match op with
      | "=" | "=="  -> Llvm.Icmp.Eq,  Llvm.Fcmp.Oeq
      | "<>" | "!=" -> Llvm.Icmp.Ne,  Llvm.Fcmp.One
      | "<"         -> Llvm.Icmp.Slt, Llvm.Fcmp.Olt
      | ">"         -> Llvm.Icmp.Sgt, Llvm.Fcmp.Ogt
      | "<="        -> Llvm.Icmp.Sle, Llvm.Fcmp.Ole
      | ">="        -> Llvm.Icmp.Sge, Llvm.Fcmp.Oge
      | _           -> assert false
  in match vl, List.map Llvm.type_of vl with
    | [v1; v2], tyl when List.exists ((=) cg.cg_i1_type) tyl ->
        let cmplhs = build_unbox v1 cg.cg_i1_type "cmplhs" cg in
        let cmprhs = build_unbox v2 cg.cg_i1_type "cmprhs" cg in
          Llvm.build_icmp (fst (op_to_cmp op)) cmplhs cmprhs "cmptmp" cg.cg_builder
    | [v1; v2], tyl when List.exists ((=) cg.cg_int_type) tyl ->
        let cmplhs = build_unbox v1 cg.cg_int_type "cmplhs" cg in
        let cmprhs = build_unbox v2 cg.cg_int_type "cmprhs" cg in
          Llvm.build_icmp (fst (op_to_cmp op)) cmplhs cmprhs "cmptmp" cg.cg_builder
    | [v1; v2], [ty1; ty2] when ty1 = cg.cg_float_type || ty2 = cg.cg_float_type ->
        let cmplhs = build_unbox v1 cg.cg_float_type "cmplhs" cg in
        let cmprhs = build_unbox v2 cg.cg_float_type "cmprhs" cg in
          Llvm.build_fcmp (snd (op_to_cmp op)) cmplhs cmprhs "cmptmp" cg.cg_builder
    | [v1; v2], [ty1; ty2] when (op = "==" || op = "!=") && ty1 = cg.cg_value_type && ty2 = cg.cg_value_type ->
        Llvm.build_icmp (fst (op_to_cmp op)) v1 v2 "cmptmp" cg.cg_builder
    | [v1; v2], _ ->
        let cmplhs = build_box v1 "cmplhs" cg in
        let cmprhs = build_box v2 "cmprhs" cg in
        let cmptmp = build_call cg.cg_compare_func [cmplhs; cmprhs] "cmptmp" cg in
          builtin_compare op [cmptmp; const_int 0 cg] cg
    | _ ->
        build_trampoline ("compare$" ^ op) 2 (builtin_compare op) cg

let rec builtin_integer (op:Id.t) (vl:Llvm.llvalue list) (cg:t): Llvm.llvalue =
  match vl with
    | [v1; v2] ->
        let integerlhs = build_unbox v1 cg.cg_int_type "integerlhs" cg in
        let integerrhs = build_unbox v2 cg.cg_int_type "integerrhs" cg in
          (match op with
             | "+"    -> Llvm.build_add
             | "-"    -> Llvm.build_sub
             | "*"    -> Llvm.build_mul
             | "land" -> Llvm.build_and
             | "lor"  -> Llvm.build_or
             | "lxor" -> Llvm.build_xor
             | "lsl"  -> Llvm.build_shl
             | "lsr"  -> Llvm.build_lshr
             | "asr"  -> Llvm.build_ashr
             | _      -> assert false) integerlhs integerrhs "integertmp" cg.cg_builder
    | _ ->
        build_trampoline ("integer$" ^ op) 2 (builtin_integer op) cg

let rec builtin_float (op:string) (vl:Llvm.llvalue list) (cg:t): Llvm.llvalue =
  match vl with
    | [v1; v2] ->
        let floatlhs = build_unbox v1 cg.cg_float_type "floatlhs" cg in
        let floatrhs = build_unbox v2 cg.cg_float_type "floatrhs" cg in
          (match op with
             | "+." -> Llvm.build_fadd
             | "-." -> Llvm.build_fsub
             | "*." -> Llvm.build_fmul
             | _    -> assert false) floatlhs floatrhs "floattmp" cg.cg_builder
    | _ ->
        build_trampoline ("float$" ^ op) 2 (builtin_float op) cg

let rec builtin_ref (vl:Llvm.llvalue list) (cg:t): Llvm.llvalue =
  match vl with
    | [v] ->
        let cell = build_tuple 1 "cell" cg in
        let pointer = build_gep cell 0 "pointer" cg in
        let boxed = build_box v "boxed" cg in
          ignore (Llvm.build_store boxed pointer cg.cg_builder);
          cell
    | _ ->
        build_trampoline "ref" 1 builtin_ref cg

let rec builtin_deref (vl:Llvm.llvalue list) (cg:t): Llvm.llvalue =
  match vl with
    | [v] ->
        build_proj v 0 "deref" cg
    | _ ->
        build_trampoline "deref" 1 builtin_deref cg

let rec builtin_assign (vl:Llvm.llvalue list) (cg:t): Llvm.llvalue =
  match vl with
    | [v1; v2] ->
        let pointer = build_gep v1 0 "pointer" cg in
        let boxed = build_box v2 "boxed" cg in
          Llvm.build_store boxed pointer cg.cg_builder
    | _ ->
        build_trampoline "assign" 2 builtin_assign cg

let rec builtin_ignore (vl:Llvm.llvalue list) (cg:t): Llvm.llvalue =
  match vl with
    | [_] ->
        const_unit cg
    | _ ->
        build_trampoline "ignore" 1 builtin_ignore cg

let builtins =
  [
    "()",     (fun _ cg -> const_unit cg);
    "true",   (fun _ cg -> Llvm.const_int cg.cg_i1_type 1);
    "false",  (fun _ cg -> Llvm.const_int cg.cg_i1_type 0);
    "=",      builtin_compare "=";
    "<>",     builtin_compare "<>";
    "<",      builtin_compare "<";
    ">",      builtin_compare ">";
    "<=",     builtin_compare "<=";
    ">=",     builtin_compare ">=";
    "==",     builtin_compare "==";
    "!=",     builtin_compare "!=";
    "+",      builtin_integer "+";
    "-",      builtin_integer "-";
    "*",      builtin_integer "*";
    "land",   builtin_integer "land";
    "lor",    builtin_integer "lor";
    "lsl",    builtin_integer "lsl";
    "lsr",    builtin_integer "lsr";
    "asr",    builtin_integer "asr";
    "+.",     builtin_float "+.";
    "-.",     builtin_float "-.";
    "*.",     builtin_float "*.";
    (* TODO *)
    "ref",    builtin_ref;
    "!",      builtin_deref;
    ":=",     builtin_assign;
    "ignore", builtin_ignore;
  ]


(**
 ** Compilation environment
 **)

type environment =
    (Id.t * (Llvm.llvalue list -> t -> Llvm.llvalue)) list

let extend (id:Id.t) (v:Llvm.llvalue) (eta:environment) =
  let f vl cg =
    match List.map (fun v -> build_box v "arg" cg) vl with
      | [] -> v
      | vl -> build_call v vl "calltmp" cg
  in (id, f) :: eta

let eta0 =
  builtins


(**
 ** Code generation
 **)

let rec generate (eta:environment) (e:Syntax.t) (cg:t): Llvm.llvalue =
  match e with
    | Syntax.Int(i) ->
        const_int i cg
    | Syntax.Char(c) ->
        const_int (int_of_char c) cg
    | Syntax.Float(f) ->
        const_float f cg
    | Syntax.String(s) ->
        assert false
    | Syntax.Ident(id) ->
        (List.assoc id eta) [] cg
    | Syntax.If(e0, e1, e2) ->
        generate_if eta e0 e1 e2 cg
    | Syntax.Tuple(el) ->
        assert false
    | Syntax.Sequence(e1, e2) ->
        ignore (generate eta e1 cg);
        generate eta e2 cg
    | Syntax.App(e, el) ->
        generate_app eta e el cg
    | Syntax.Abstr(idl, e) ->
        generate_abstr eta None idl e cg
    | Syntax.Let(id, e1, e2) ->
        generate (extend id (generate eta e1 cg) eta) e2 cg
    | Syntax.LetTuple(idl, e1, e2) ->
        assert false
    | Syntax.LetRec(id, e1, e2) ->
        generate_letrec eta id e1 e2 cg
and generate_app eta e el cg =
  match e with
    | Syntax.Ident(id) ->
        let vl = List.map (fun e -> generate eta e cg) el in
          (List.assoc id eta) vl cg
    | e ->
        let fn = generate eta e cg in
        let args = List.map (fun e -> build_box (generate eta e cg) "argtmp" cg) el in
          build_call fn args "calltmp" cg
and generate_abstr eta id idl e cg =
  let fnname = (match id with Some(id) -> id | None -> "anonymous") in
  let fnbody fn vl cg =
    let eta' = (match id with Some(id) -> extend id fn eta | None -> eta) in
    let eta'' = List.fold_right2 (fun id v eta -> Llvm.set_value_name id v; extend id v eta) idl vl eta' in
      generate eta'' e cg
  in
    build_function fnname (List.length idl) fnbody cg
and generate_if eta e0 e1 e2 cg =
  let ifcond = build_unbox (generate eta e0 cg) cg.cg_i1_type "ifcond" cg in
  let bb = Llvm.insertion_block cg.cg_builder in
  (* generate e1 *)
  let e1_bb = Llvm.append_block cg.cg_context "then" (Llvm.block_parent bb) in
  let _ = Llvm.position_at_end e1_bb cg.cg_builder in
  let e1_val_raw = generate eta e1 cg in
  let e1_bb_end = Llvm.insertion_block cg.cg_builder in
  (* generate e2 *)
  let e2_bb = Llvm.append_block cg.cg_context "else" (Llvm.block_parent bb) in
  let _ = Llvm.position_at_end e2_bb cg.cg_builder in
  let e2_val_raw = generate eta e2 cg in
  let e2_bb_end = Llvm.insertion_block cg.cg_builder in
  (* generate ifcont and check boxing *)
  let ifcont_bb = Llvm.append_block cg.cg_context "ifcont" (Llvm.block_parent bb) in
  let box_required = Llvm.type_of e1_val_raw <> Llvm.type_of e2_val_raw in
  (* box e1 if required and br to ifcont *)
  let _ = Llvm.position_at_end e1_bb_end cg.cg_builder in
  let e1_val = if box_required then build_box e1_val_raw "thentmp" cg else e1_val_raw in
  let e1_bb_phi = Llvm.insertion_block cg.cg_builder in
  let _ = Llvm.build_br ifcont_bb cg.cg_builder in
  (* box e2 if required and br to ifcont *)
  let _ = Llvm.position_at_end e2_bb_end cg.cg_builder in
  let e2_val = if box_required then build_box e2_val_raw "elsetmp" cg else e2_val_raw in
  let e2_bb_phi = Llvm.insertion_block cg.cg_builder in
  let _ = Llvm.build_br ifcont_bb cg.cg_builder in
  (* build the phi *)
  let _ = Llvm.position_at_end ifcont_bb cg.cg_builder in
  let iftmp = Llvm.build_phi [(e1_val, e1_bb_phi); (e2_val, e2_bb_phi)] "iftmp" cg.cg_builder in
  (* build the condbr *)
  let _ = Llvm.position_at_end bb cg.cg_builder in
  let _ = Llvm.build_cond_br ifcond e1_bb e2_bb cg.cg_builder in
    Llvm.position_at_end ifcont_bb cg.cg_builder;
    iftmp
and generate_letrec eta id e1 e2 cg =
  match e1 with
    | Syntax.Abstr(idl, e1) ->
        let fn = generate_abstr eta (Some(id)) idl e1 cg in
          generate (extend id fn eta) e2 cg
    | _ ->
        raise (Invalid_argument("Codegen.generate_letrec"))

let dump (e:Syntax.t) (cg:t): unit =
  let maincode _ _ cg = build_unbox (generate eta0 e cg) cg.cg_int_type "result" cg in
  let maintype = Llvm.function_type cg.cg_int_type [||] in
    ignore (build_native_function "main" maintype maincode cg);
    Llvm.dump_module cg.cg_module


(**
 ** Context management
 **)

let create (name:string): t =
  let cg_context = Llvm.create_context () in
  let cg_module = Llvm.create_module cg_context name in
  let cg_builder = Llvm.builder cg_context in
  let cg_fpmanager = Llvm.PassManager.create_function cg_module in
  let cg_i1_type = Llvm.i1_type cg_context in
  let cg_int_type = Llvm.integer_type cg_context Sys.word_size in
  let cg_float_type = Llvm.double_type cg_context in
  let cg_value_type = Llvm.pointer_type (Llvm.pointer_type cg_int_type) in
  let cg_floatbox_type = Llvm.struct_type cg_context [|cg_int_type; cg_float_type|] in
  let cg_alloc_float_func = (Llvm.declare_function
                               "mincaml2_alloc_float"
                               (Llvm.function_type cg_value_type [|cg_float_type|])
                               cg_module) in
  let cg_alloc_tuple_func = (Llvm.declare_function
                               "mincaml2_alloc_tuple"
                               (Llvm.function_type cg_value_type [|cg_int_type|])
                               cg_module) in
  let cg_compare_func = (Llvm.declare_function
                           "mincaml2_compare"
                           (Llvm.function_type cg_value_type [|cg_value_type; cg_value_type|])
                           cg_module) in
    (* TODO *)
    Llvm.set_function_call_conv Llvm.CallConv.fast cg_alloc_float_func;
    Llvm.set_function_call_conv Llvm.CallConv.fast cg_alloc_tuple_func;
    Llvm.set_function_call_conv Llvm.CallConv.fast cg_compare_func;
    ignore (Llvm.define_type_name "floatbox" cg_floatbox_type cg_module);
    ignore (Llvm.define_type_name "value" cg_value_type cg_module);
    Llvm_scalar_opts.add_instruction_combination cg_fpmanager;
    Llvm_scalar_opts.add_reassociation cg_fpmanager;
    Llvm_scalar_opts.add_gvn cg_fpmanager;
    Llvm_scalar_opts.add_cfg_simplification cg_fpmanager;
    ignore (Llvm.PassManager.initialize cg_fpmanager);
    { cg_context = cg_context;
      cg_module = cg_module;
      cg_builder = cg_builder;
      cg_fpmanager = cg_fpmanager;
      cg_i1_type = cg_i1_type;
      cg_int_type = cg_int_type;
      cg_float_type = cg_float_type;
      cg_floatbox_type = cg_floatbox_type;
      cg_value_type = cg_value_type;
      cg_alloc_float_func = cg_alloc_float_func;
      cg_alloc_tuple_func = cg_alloc_tuple_func;
      cg_compare_func = cg_compare_func }


