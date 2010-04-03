open Astcommon
open Lambda
open Primitive

module IdentMap = Map.Make(Ident)

type t =
    { cg_context:         Llvm.llcontext;
      cg_module:          Llvm.llmodule;
      cg_builder:         Llvm.llbuilder;
      cg_manager:         [`Function] Llvm.PassManager.t;

      cg_i1_type:         Llvm.lltype;
      cg_i8_type:         Llvm.lltype;
      cg_i32_type:        Llvm.lltype;
      cg_int_type:        Llvm.lltype;
      cg_float_type:      Llvm.lltype;
      cg_value_type:      Llvm.lltype;
      cg_floatblk_type:   Llvm.lltype;
    
      cg_alloc_func:      Llvm.llvalue }


(*********************)
(*** Miscellaneous ***)
(*********************)

let const_i8 n cg =
  Llvm.const_int cg.cg_i8_type n

let const_i32 n cg =
  Llvm.const_int cg.cg_i32_type n

let const_int n cg =
  Llvm.const_int cg.cg_int_type n

let const_pointer n cg =
  Llvm.const_inttoptr (Llvm.const_of_int64 cg.cg_int_type (Int64.of_nativeint n) true) cg.cg_value_type

let const_float f cg =
  Llvm.const_float_of_string cg.cg_float_type f

let const_string s cg =
  let slen = String.length s in
  let wobsize = Sys.word_size / 8 in
  let swosize = (slen + wobsize) / wobsize in
  let varray = Array.make (swosize * wobsize) (const_i8 0 cg) in
  for i = 0 to slen - 1 do varray.(i) <- const_i8 (int_of_char (String.get s i)) cg done;
  let vhdr = const_pointer (Lambda.make_header Lambda.tag_string swosize) cg in
  let v = Llvm.const_struct cg.cg_context [|vhdr; Llvm.const_array cg.cg_i8_type varray|] in
    Llvm.define_global "" v cg.cg_module

let build_box v cg =
  match Llvm.type_of v with
    | ty when ty = cg.cg_value_type ->
        v
    | ty when ty = cg.cg_i1_type ->
        let v = Llvm.build_sext v cg.cg_int_type "" cg.cg_builder in
        let v = Llvm.build_or v (const_int 1 cg) "" cg.cg_builder in
          Llvm.build_inttoptr v cg.cg_value_type "" cg.cg_builder
    | ty when ty = cg.cg_int_type ->
        let v = Llvm.build_shl v (const_int 1 cg) "" cg.cg_builder in
        let v = Llvm.build_or v (const_int 1 cg) "" cg.cg_builder in
          Llvm.build_inttoptr v cg.cg_value_type "" cg.cg_builder
    | ty when ty = cg.cg_float_type && Llvm.is_constant v ->
        let vwosize = 8 / ((Sys.word_size / 8)) in
        let vhdr = const_pointer (Lambda.make_header Lambda.tag_float vwosize) cg in
        let v = Llvm.const_struct cg.cg_context (Array.of_list [vhdr; v]) in
        let v = Llvm.define_global "" v cg.cg_module in
          Llvm.set_global_constant true v;
          Llvm.set_linkage Llvm.Linkage.Private v;
          Llvm.const_pointercast v cg.cg_value_type
    | ty when ty = cg.cg_float_type ->
        let vwosize = 8 / ((Sys.word_size / 8)) in
        let vhdr = const_pointer (Lambda.make_header Lambda.tag_float vwosize) cg in
        let vptr = Llvm.build_call cg.cg_alloc_func [|vhdr|] "" cg.cg_builder in
        let fbptr = Llvm.build_pointercast vptr (Llvm.pointer_type cg.cg_floatblk_type) "" cg.cg_builder in
        let fptr = Llvm.build_gep fbptr [|const_i32 0 cg; const_i32 1 cg|] "" cg.cg_builder in
          ignore (Llvm.build_store v fptr cg.cg_builder);
          vptr
    | ty when Llvm.classify_type ty = Llvm.TypeKind.Pointer ->
        Llvm.build_pointercast v cg.cg_value_type "" cg.cg_builder
    | ty ->
        failwith ("Cannot box value of type " ^ (Llvm.string_of_lltype ty))

let build_unbox v ty cg =
  match Llvm.type_of v, ty with
    | vty, ty when vty = ty ->
        v
    | vty, ty when vty = cg.cg_value_type && ty = cg.cg_i1_type ->
        Llvm.build_icmp Llvm.Icmp.Ne v (const_pointer 1n cg) "" cg.cg_builder
    | vty, ty when vty = cg.cg_value_type && ty = cg.cg_int_type ->
        let v = Llvm.build_ptrtoint v cg.cg_int_type "" cg.cg_builder in
          Llvm.build_ashr v (const_int 1 cg) "" cg.cg_builder
    | vty, ty when vty = cg.cg_value_type && ty = cg.cg_float_type ->
        let v = Llvm.build_pointercast v (Llvm.pointer_type cg.cg_floatblk_type) "" cg.cg_builder in
        let v = Llvm.build_gep v [|const_i32 0 cg; const_i32 1 cg|] "" cg.cg_builder in
          Llvm.build_load v "" cg.cg_builder
    | vty, ty when vty = cg.cg_value_type && Llvm.classify_type ty = Llvm.TypeKind.Pointer ->
        Llvm.build_pointercast v ty "" cg.cg_builder
    | ty, vty ->
        failwith ("Cannot unbox from " ^ (Llvm.string_of_lltype vty) ^ " to " ^ (Llvm.string_of_lltype ty))

let build_phi phil cg =
  match phil with
    | [] ->
        (* dead phi switch *)
        Llvm.undef cg.cg_value_type
    | (v1, _) :: _ ->
        (* allocate the basicblock for the phi *)
        let bbphi = Llvm.append_block cg.cg_context "phi" (Llvm.block_parent (Llvm.insertion_block cg.cg_builder)) in
        (* check if we need to box the values *)
        let boxing = List.exists (fun (v, _) -> Llvm.type_of v <> Llvm.type_of v1) phil in
        (* generate the (optional) boxing and the br's to the phi node *)
        let phil = (List.map
                      (fun (v, bb) ->
                         Llvm.position_at_end bb cg.cg_builder;
                         let v = if boxing then build_box v cg else v in
                           ignore (Llvm.build_br bbphi cg.cg_builder);
                           v, Llvm.insertion_block cg.cg_builder)
                      phil) in
          (* generate the actual phi *)
          Llvm.position_at_end bbphi cg.cg_builder;
          Llvm.build_phi phil "" cg.cg_builder
                

(***********************)
(*** Code generation ***)
(***********************)

let rec generate cg env cont = function
  | Lconst(sc) ->
      generate_sconst cg sc
  | Lident(id) ->
      IdentMap.find id env
  | Lapply(lambda, lambdal) ->
      let v = generate cg env cont lambda in
      let vl = List.map (fun lambda -> build_box (generate cg env cont lambda) cg) lambdal in
      let vty = Llvm.function_type cg.cg_value_type (Array.make (List.length vl) cg.cg_value_type) in
      let v = Llvm.build_pointercast v (Llvm.pointer_type vty) "" cg.cg_builder in
      let v = Llvm.build_call v (Array.of_list vl) "" cg.cg_builder in
        Llvm.set_instruction_call_conv Llvm.CallConv.fast v;
        Llvm.set_tail_call true v;
        v
  | Lfunction(_) ->
      assert false
  | Llet(id, lambda1, lambda2) ->
      generate cg (IdentMap.add id (generate cg env cont lambda1) env) cont lambda2
  | Lletrec(idlambdal, lambda) ->
      generate_letrec cg env cont idlambdal lambda
  | Lprim(p, lambdal) ->
      generate_prim cg env cont p lambdal
  | Lswitch(lambda, switch) ->
      generate_switch cg env cont lambda switch
  | Lstaticraise ->
      begin match cont with
        | None -> assert false
        | Some(bb) -> Llvm.build_br bb cg.cg_builder
      end
  | Lstaticcatch(lambda1, lambda2) ->
      (* setup the basicblocks for lambda1/lambda2 *)
      let bb1 = Llvm.insertion_block cg.cg_builder in
      let bb2 = Llvm.append_block cg.cg_context "catch" (Llvm.block_parent bb1) in
      (* generate code for lambda1 with lambda2 as continuation *)
      Llvm.position_at_end bb1 cg.cg_builder;
      let v1 = generate cg env (Some(bb2)) lambda1 in
      let phil = [v1, Llvm.insertion_block cg.cg_builder] in
      (* generate code for lambda2 *)
      Llvm.position_at_end bb2 cg.cg_builder;
      let v2 = generate cg env cont lambda2 in
      let phil = (v2, Llvm.insertion_block cg.cg_builder) :: phil in
      (* generate the phi block *)
        build_phi phil cg
  | Ltrywith(_) ->
      assert false (* TODO *)
  | Lifthenelse(lambda0, lambda1, lambda2) ->
      (* setup the basicblocks for lambda0/lambda1/lambda2 *)
      let bb0 = Llvm.insertion_block cg.cg_builder in
      let bb1 = Llvm.append_block cg.cg_context "then" (Llvm.block_parent bb0) in
      let bb2 = Llvm.append_block cg.cg_context "else" (Llvm.block_parent bb0) in
      (* generate code for lambda0 *)
      Llvm.position_at_end bb0 cg.cg_builder;
      let v0 = build_unbox (generate cg env cont lambda0) cg.cg_i1_type cg in
      ignore (Llvm.build_cond_br v0 bb1 bb2 cg.cg_builder);
      (* generate code for lambda1 *)
      Llvm.position_at_end bb1 cg.cg_builder;
      let v1 = generate cg env cont lambda1 in
      let phil = [v1, Llvm.insertion_block cg.cg_builder] in
      (* generate code for lambda2 *)
      Llvm.position_at_end bb2 cg.cg_builder;
      let v2 = generate cg env cont lambda2 in
      let phil = (v2, Llvm.insertion_block cg.cg_builder) :: phil in
      (* generate the phi block *)
        build_phi phil cg
  | Lsequence(lambda1, lambda2) ->
      ignore (generate cg env cont lambda1);
      generate cg env cont lambda2

and generate_switch cg env cont lambda switch =
  let v0 = build_box (generate cg env cont lambda) cg in
  (* determine the initial basicblock *)
  let bb0 = Llvm.insertion_block cg.cg_builder in
  (* allocate the continuation block for the const switch *)
  let bb1 = Llvm.append_block cg.cg_context "consts" (Llvm.block_parent bb0) in
  (* allocate the continuation block for the block switch *)
  let bb2 = Llvm.append_block cg.cg_context "blocks" (Llvm.block_parent bb0) in
  (* initialize the phi collector list *)
  let phil = ref [] in
  (* setup the const switch *)
  Llvm.position_at_end bb0 cg.cg_builder;
  let v1 = Llvm.build_ptrtoint v0 cg.cg_int_type "" cg.cg_builder in
  let sw1 = Llvm.build_switch v1 bb1 switch.sw_numconsts cg.cg_builder in
    (List.iter
       (fun (tag, lambda) ->
          let bb = Llvm.append_block cg.cg_context "case" (Llvm.block_parent bb0) in
          let tag = Llvm.const_or (Llvm.const_shl (const_int tag cg) (const_int 1 cg)) (const_int 1 cg) in
            Llvm.add_case sw1 tag bb;
            Llvm.position_at_end bb cg.cg_builder;
            let v = generate cg env cont lambda in
              phil := (v, Llvm.insertion_block cg.cg_builder) :: !phil)
       switch.sw_consts);
  (* setup the block switch *)
  Llvm.position_at_end bb1 cg.cg_builder;
  let v2 = Llvm.build_load v0 "" cg.cg_builder in
  let v2 = Llvm.build_and v2 (const_int 0xff cg) "" cg.cg_builder in
  let sw2 = Llvm.build_switch v2 bb2 switch.sw_numblocks cg.cg_builder in
    (List.iter
       (fun (tag, lambda) ->
          let bb = Llvm.append_block cg.cg_context "case" (Llvm.block_parent bb0) in
            Llvm.add_case sw2 (const_int tag cg) bb;
            Llvm.position_at_end bb cg.cg_builder;
            let v = generate cg env cont lambda in
              phil := (v, Llvm.insertion_block cg.cg_builder) :: !phil)
       switch.sw_blocks);
  (* setup the default case *)
  Llvm.position_at_end bb2 cg.cg_builder;
  begin match switch.sw_default with
    | None ->
        ignore (Llvm.build_unreachable cg.cg_builder)
    | Some(lambda) ->
        let v = generate cg env cont lambda in
          phil := (v, Llvm.insertion_block cg.cg_builder) :: !phil
  end;
  (* build the phi node *)
  build_phi !phil cg
  
and generate_letrec cg env cont idlambdal lambda =
  let bb = Llvm.insertion_block cg.cg_builder in
  let idfl = (List.map
                (function
                   | (id, Lfunction(idl, lambda)) ->
                       let farity = List.length idl in
                       let ftype = Llvm.function_type cg.cg_value_type (Array.make farity cg.cg_value_type) in
                       let fval = Llvm.declare_function (Ident.unique_name id) ftype cg.cg_module in
                         (Array.iteri
                            (fun i v -> Llvm.set_value_name (Ident.unique_name (List.nth idl i)) v)
                            (Llvm.params fval));
                         Llvm.set_function_call_conv Llvm.CallConv.fast fval;
                         Llvm.set_linkage Llvm.Linkage.Private fval;
                         (id, idl, lambda, fval)
                   | _ -> assert false)
                idlambdal) in
  let env = (List.fold_left
               (fun env (id, _, _, fval) -> IdentMap.add id fval env)
               env
               idfl) in
    (List.iter
       (fun (id, idl, lambda, fval) ->
          let entry = Llvm.append_block cg.cg_context "entry" fval in
          let env = (List.fold_left2
                       (fun env id v -> IdentMap.add id v env)
                       env idl (Array.to_list (Llvm.params fval))) in
            Llvm.position_at_end entry cg.cg_builder;
            let v = build_box (generate cg env None lambda) cg in
              ignore (Llvm.build_ret v cg.cg_builder);
              Llvm_analysis.assert_valid_function fval;
              ignore (Llvm.PassManager.run_function fval cg.cg_manager))
       idfl);
    Llvm.position_at_end bb cg.cg_builder;
    generate cg env cont lambda

and generate_prim cg env cont p lambdal =
  match p, List.map (generate cg env cont) lambdal with
    | Pignore, [v] ->
        const_int 0 cg
    | Pidentity, [v] ->
        v
    | Praise, _ ->
        assert false (* TODO *)
    | Pcompare, _ ->
        assert false (* TODO *)
    | Pmakeblock(header, Immutable), vl ->
        let vl = List.map (fun v -> build_box v cg) vl in
          if List.for_all (fun v -> Llvm.is_constant v) vl then begin
            let vhdr = const_pointer header cg in
            let v = Llvm.const_array cg.cg_value_type (Array.of_list (vhdr :: vl)) in
            let v = Llvm.define_global "" v cg.cg_module in
              Llvm.set_global_constant true v;
              Llvm.set_linkage Llvm.Linkage.Private v;
              v
          end else begin
            let vhdr = const_pointer header cg in
            let v = Llvm.build_call cg.cg_alloc_func [|vhdr|] "" cg.cg_builder in
            let vptr = Llvm.build_pointercast v (Llvm.pointer_type cg.cg_value_type) "" cg.cg_builder in
              (Array.iteri
                 (fun i v ->
                    let vptr = Llvm.build_gep vptr [|const_i32 (i + 1) cg|] "" cg.cg_builder in
                      ignore (Llvm.build_store v vptr cg.cg_builder))
                 (Array.of_list vl));
              v
          end
    | Pmakeblock(_), _ ->
        assert false (* TODO *)
    | Pfield(n), [v] ->
        let v = Llvm.build_pointercast v (Llvm.pointer_type cg.cg_value_type) "" cg.cg_builder in
        let v = Llvm.build_gep v [|const_i32 (n + 1) cg|] "" cg.cg_builder in
          Llvm.build_load v "" cg.cg_builder
    | Poffset(n), [v] ->
        Llvm.build_gep (build_box v cg) [|const_int n cg|] "" cg.cg_builder
    | Pextcall(prim), vl ->
        let vl, ty = (if prim.prim_native_float then begin
                        List.map (fun v -> build_unbox v cg.cg_float_type cg) vl,
                        cg.cg_float_type
                      end else begin
                        List.map (fun v -> build_box v cg) vl,
                        cg.cg_value_type
                      end) in
        let fname = (if prim.prim_native_name <> "" then prim.prim_native_name else prim.prim_name) in
        let ftype = Llvm.function_type ty (Array.make prim.prim_arity ty) in
        let faddr = Llvm.declare_function fname ftype cg.cg_module in
          Llvm.build_call faddr (Array.of_list vl) "" cg.cg_builder
    | Pnegint, [v1] ->
        Llvm.build_neg (build_unbox v1 cg.cg_int_type cg) "" cg.cg_builder
    | (Paddint | Psubint | Pmulint  | Pdivint | Pmodint
      | Pandint | Porint | Pxorint | Plslint | Plsrint | Pasrint as p), [v1; v2] ->
        let v1 = build_unbox v1 cg.cg_int_type cg in
        let v2 = build_unbox v2 cg.cg_int_type cg in
          (match p with
             | Paddint -> Llvm.build_add
             | Psubint -> Llvm.build_sub
             | Pmulint -> Llvm.build_mul
             | Pdivint -> Llvm.build_sdiv
             | Pmodint -> Llvm.build_srem
             | Pandint -> Llvm.build_and
             | Porint  -> Llvm.build_or
             | Pxorint -> Llvm.build_xor
             | Plslint -> Llvm.build_shl
             | Plsrint -> Llvm.build_lshr 
             | Pasrint -> Llvm.build_ashr
             | _ -> assert false) v1 v2 "" cg.cg_builder
    | Pintcmp(cmp), [v1; v2] ->
        let v1, v2 = (if Llvm.type_of v1 = cg.cg_int_type && Llvm.type_of v2 = cg.cg_int_type then
                        v1, v2
                      else
                        build_box v1 cg, build_box v2 cg) in
        let icmp = (match cmp with
                      | Ceq -> Llvm.Icmp.Eq  | Cne -> Llvm.Icmp.Ne
                      | Clt -> Llvm.Icmp.Slt | Cgt -> Llvm.Icmp.Sgt
                      | Cle -> Llvm.Icmp.Sle | Cge -> Llvm.Icmp.Sge) in
          Llvm.build_icmp icmp v1 v2 "" cg.cg_builder
    | Pintoffloat, [v] ->
        let v = build_unbox v cg.cg_int_type cg in
          Llvm.build_sitofp v cg.cg_float_type "" cg.cg_builder
    | Pfloatofint, [v] ->
        let v = build_unbox v cg.cg_float_type cg in
          Llvm.build_fptosi v cg.cg_int_type "" cg.cg_builder
    | Pnegfloat, [v] ->
        Llvm.build_fneg (build_unbox v cg.cg_float_type cg) "" cg.cg_builder
    | (Paddfloat | Psubfloat | Pmulfloat | Pdivfloat as p), [v1; v2] ->
        let v1 = build_unbox v1 cg.cg_float_type cg in
        let v2 = build_unbox v2 cg.cg_float_type cg in
          (match p with
             | Paddfloat -> Llvm.build_fadd
             | Psubfloat -> Llvm.build_fsub
             | Pmulfloat -> Llvm.build_fmul
             | Pdivfloat -> Llvm.build_fdiv
             | _ -> assert false) v1 v2 "" cg.cg_builder
    | _ ->
        assert false (* TODO *)

and generate_sconst cg = function
  | Sconst_base(c) ->
      generate_const cg c
  | Sconst_pointer(a) ->
      const_pointer a cg
  | Sconst_immstring(s) ->
      let v = const_string s cg in
        Llvm.set_global_constant true v;
        Llvm.set_linkage Llvm.Linkage.Internal v;
        v
  | Sconst_block(header, scl) ->
      let vl = List.map (fun sc -> build_box (generate_sconst cg sc) cg) scl in
      let vhdr = const_pointer header cg in
      let v = Llvm.const_array cg.cg_value_type (Array.of_list (vhdr :: vl)) in
      let v = Llvm.define_global "" v cg.cg_module in
        Llvm.set_global_constant true v;
        Llvm.set_linkage Llvm.Linkage.Private v;
        v

and generate_const cg = function
  | Const_int(n) ->
      const_int n cg
  | Const_char(c) ->
      const_int (int_of_char c) cg
  | Const_float(f) ->
      const_float f cg
  | Const_int32(_) ->
      assert false (* TODO *)
  | Const_int64(_) ->
      assert false (* TODO *)
  | Const_string(s) ->
      let v = const_string s cg in
        Llvm.set_linkage Llvm.Linkage.Private v;
        v
  | Const_nativeint(_) ->
      assert false (* TODO *)


let create () =
  let cg_context = Llvm.global_context () in
  let cg_module = Llvm.create_module cg_context "mincaml2" in
  let cg_builder = Llvm.builder cg_context in
  let cg_manager = Llvm.PassManager.create_function cg_module in
  let cg_i1_type = Llvm.i1_type cg_context in
  let cg_i8_type = Llvm.i8_type cg_context in
  let cg_i32_type = Llvm.i32_type cg_context in
  let cg_int_type = Llvm.integer_type cg_context Sys.word_size in
  let cg_float_type = Llvm.double_type cg_context in
  let cg_value_type = Llvm.pointer_type cg_int_type in
  let cg_floatblk_type = Llvm.struct_type cg_context [|cg_value_type; cg_float_type|] in
  let cg_alloc_func = Llvm.declare_function "mc2_alloc" (Llvm.function_type cg_value_type [|cg_value_type|]) cg_module in
    ignore (Llvm.define_type_name "value" cg_value_type cg_module);
    ignore (Llvm.define_type_name "floatblk" cg_floatblk_type cg_module);
    Llvm_scalar_opts.add_instruction_combination cg_manager;
    Llvm_scalar_opts.add_reassociation cg_manager;
    Llvm_scalar_opts.add_gvn cg_manager;
    Llvm_scalar_opts.add_cfg_simplification cg_manager;
    Llvm_scalar_opts.add_jump_threading cg_manager;
    ignore (Llvm.PassManager.initialize cg_manager);
    { cg_context = cg_context;
      cg_module = cg_module;
      cg_builder = cg_builder;
      cg_manager = cg_manager;
      cg_i1_type = cg_i1_type;
      cg_i8_type = cg_i8_type;
      cg_i32_type = cg_i32_type;
      cg_int_type = cg_int_type;
      cg_float_type = cg_float_type;
      cg_value_type = cg_value_type;
      cg_floatblk_type = cg_floatblk_type;
      cg_alloc_func = cg_alloc_func }

let generate_predefined_exn id cg =
  let slen = String.length (Ident.name id) in
  let slen = ((slen + (Sys.word_size / 8)) / (Sys.word_size / 8)) * (Sys.word_size / 8) in
  let sty = Llvm.struct_type cg.cg_context [|cg.cg_value_type; (Llvm.array_type cg.cg_i8_type slen)|] in
  let v = Llvm.declare_global sty (Ident.unique_name id) cg.cg_module in
    Llvm.set_global_constant true v;
    Llvm.const_pointercast v cg.cg_value_type


(********************)
(*** Entry points ***)
(********************)

let dump lambda =
  let cg = create () in
  (* generate the predefined exceptions *)
  let env0 = (List.fold_left
                (fun env id -> IdentMap.add id (generate_predefined_exn id cg) env)
                IdentMap.empty
                [Typeenv.ident_match_failure;
                 Typeenv.ident_out_of_memory;
                 Typeenv.ident_stack_overflow;
                 Typeenv.ident_invalid_argument;
                 Typeenv.ident_failure;
                 Typeenv.ident_not_found;
                 Typeenv.ident_division_by_zero]) in
  (* generate the "entry" for this module *)
  let entry = Llvm.declare_function "mc2_entry" (Llvm.function_type (Llvm.void_type cg.cg_context) [||]) cg.cg_module in
  let bb = Llvm.append_block cg.cg_context "mc2_entry" entry in
    Llvm.position_at_end bb cg.cg_builder;
    ignore (generate cg env0 None lambda);
    ignore (Llvm.build_ret_void cg.cg_builder);
    Llvm_analysis.assert_valid_function entry;
    ignore (Llvm.PassManager.run_function entry cg.cg_manager);
    Llvm_analysis.assert_valid_module cg.cg_module;
    begin
      let mpm = Llvm.PassManager.create () in
        ignore (Llvm.PassManager.run_module cg.cg_module mpm)
    end;
    Llvm.dump_module cg.cg_module;
    ignore (Llvm_bitwriter.write_bitcode_file cg.cg_module "test.bc")
