(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  let string_of_note note = note in
  let string_of_phrase phrase = phrase in (* Fix these with guitar tab representation*)

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Handl" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context in
  let i32_ptr_t  = L.pointer_type i32_t
  and i8_ptr_t =  L.pointer_type i8_t
  and float_t    = L.double_type context in
  let str_t      = L.pointer_type i8_t in
  (* Note is a struct of a string representing pitch and int representing strength of the note *)

  (* Return the LLVM type for a primitive Handl type *)
  let ltype_of_primitive_typ = function
      A.PrimitiveType(A.Int)   -> i32_t
    | A.PrimitiveType(A.Bool)  -> i1_t
    | A.PrimitiveType(A.Float)  -> float_t
    | A.PrimitiveType(A.String)  -> str_t
    | A.PrimitiveType(A.Note)  -> str_t 
    | _                        -> raise (Failure "Unmatched type in ltype_of_typ")
  in
  (* Return the LLVM type for all Handl types *)
  let rec ltype_of_typ = function
    A.PrimitiveType(t) -> ltype_of_primitive_typ(A.PrimitiveType(t))
    | A.PrimArray(t) -> L.pointer_type (ltype_of_primitive_typ (A.PrimitiveType(t)))
    | A.PhraseType -> L.pointer_type (ltype_of_primitive_typ (A.PrimitiveType(A.Note)))
    | A.SongType -> L.pointer_type (ltype_of_typ A.PhraseType)
in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with 
          A.PrimitiveType(A.Float) -> L.const_float (ltype_of_primitive_typ t) 0.0
          | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder 
    and bool_format_str = L.build_global_stringptr "%B\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and chr_format_str = L.build_global_stringptr "%c\n" "fmt" builder 
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and note_format_str = L.build_global_stringptr "/%s/ /%g/\n" "fmt" builder
  in 

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    let elem_size_offset = L.const_int i32_t (-3)
    and size_offset = L.const_int i32_t (-2)
    and len_offset = L.const_int i32_t (-1)
    and metadata_sz = L.const_int i32_t 12 in  (* 12 bytes overhead *)

    let put_meta body_ptr offset llval builder =
      let ptr = L.build_bitcast body_ptr i32_ptr_t "i32_ptr_t" builder in
      let meta_ptr = L.build_gep ptr [| offset |] "meta_ptr" builder in
      L.build_store llval meta_ptr builder
    in

    let get_meta body_ptr offset builder =
      let ptr = L.build_bitcast body_ptr i32_ptr_t "i32_ptr_t" builder in
      let meta_ptr = L.build_gep ptr [| offset |] "meta_ptr" builder in
      L.build_load meta_ptr "meta_data" builder
    in

    let meta_to_body meta_ptr builder =
      let ptr = L.build_bitcast meta_ptr i8_ptr_t "meta_ptr" builder in
      L.build_gep ptr [| (L.const_int i8_t (12)) |] "body_ptr" builder
    in

    let body_to_meta body_ptr builder =
      let ptr = L.build_bitcast body_ptr i8_ptr_t "body_ptr" builder in
      L.build_gep ptr [| (L.const_int i8_t (-12)) |] "meta_ptr" builder
    in
    let play_song_t : L.lltype =
      L.var_arg_function_type i32_t [| ltype_of_typ A.SongType |] in
    let play_song_func : L.llvalue =
      L.declare_function "play_song" play_song_t the_module in

    (* make array *)
    let make_array element_t len builder =
      let element_sz = L.build_bitcast (L.size_of element_t) i32_t "b" builder in
      let body_sz = L.build_mul element_sz len "body_sz" builder in
      let malloc_sz = L.build_add body_sz metadata_sz "make_array_sz" builder in
      let meta_ptr = L.build_array_malloc i8_t malloc_sz "make_array" builder in
      let body_ptr = meta_to_body meta_ptr builder in
      ignore (put_meta body_ptr elem_size_offset element_sz builder);
      ignore (put_meta body_ptr size_offset malloc_sz builder);
      ignore (put_meta body_ptr len_offset len builder);
      L.build_bitcast body_ptr (L.pointer_type element_t) "make_array_ptr" builder
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i  
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      | SStrLit l -> L.build_global_stringptr (l ^ "\x00") "str_ptr" builder
      | SNewArr (t, e) -> let len = build_expr builder e
                                  in make_array (ltype_of_primitive_typ (A.PrimitiveType(t))) (len) builder
      | SId(s) -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = build_expr builder e in
               ignore(L.build_store e' (lookup s) builder); e'
      | SNoteAssign(s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.PrimitiveType(Float), _) as e1, op, e2) ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in
          (match op with
             A.Add     -> L.build_fadd
           | A.Sub     -> L.build_fsub
           | A.Mult    -> L.build_fmul
           | A.Div     -> L.build_fdiv
           | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
           | A.Neq     -> L.build_fcmp L.Fcmp.One
           | A.Less    -> L.build_fcmp L.Fcmp.Olt
           | A.Greater -> L.build_fcmp L.Fcmp.Ogt
           | A.LessEqual -> L.build_fcmp L.Fcmp.Ole
           | A.GreaterEqual -> L.build_fcmp L.Fcmp.Oge
          )e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
             A.Add     -> L.build_add
           | A.Sub     -> L.build_sub
           | A.Mult    -> L.build_mul
           | A.Div     -> L.build_sdiv
           | A.And     -> L.build_and
           | A.Or      -> L.build_or
           | A.Equal   -> L.build_icmp L.Icmp.Eq
           | A.Neq     -> L.build_icmp L.Icmp.Ne
           | A.Less    -> L.build_icmp L.Icmp.Slt
           | A.Greater -> L.build_icmp L.Icmp.Sgt
           | A.LessEqual -> L.build_icmp L.Icmp.Sle
           | A.GreaterEqual -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
      | SNot(e) ->
        let e' = build_expr builder e in L.build_not e' "tmp" builder
      | SArrAssign (arr_name, idx_expr, val_expr) ->
        let idx = (build_expr builder idx_expr)
        and assign_val = (build_expr builder val_expr) in
        let llname = arr_name ^ "[" ^ L.string_of_llvalue idx ^ "]" in
        let arr_ptr = lookup arr_name in
        let arr_ptr_load = L.build_load arr_ptr arr_name builder in
        let arr_gep = L.build_in_bounds_gep arr_ptr_load [|idx|] llname builder in
        L.build_store assign_val arr_gep builder
      | SArrAccess (arr_name, idx_expr) ->
        let idx = build_expr builder idx_expr in
        let llname = arr_name ^ "[" ^ L.string_of_llvalue idx ^ "]" in
        let arr_ptr_load =
            let arr_ptr = lookup arr_name in
            L.build_load arr_ptr arr_name builder in
        let arr_gep = L.build_in_bounds_gep arr_ptr_load [|idx|] llname builder in
          L.build_load arr_gep (llname ^ "_load") builder
      | SPhraseAssign(id) -> let t = A.PhraseType in
          let len = (A.PrimitiveType(A.Int), SLiteral(8)) in
          let newArrSexpr = SNewArr(A.Note, len) in
          let e = build_expr builder (t, newArrSexpr) in
          ignore(L.build_store e (lookup id) builder); e
      | SPhraseAdd(id, idx, note) -> 
        let t = A.PhraseType in 
        build_expr builder (t, SArrAssign(id, idx, note))
      | SSongAssign(id) -> let t = A.SongType in
        let len = build_expr builder (A.PrimitiveType(A.Int), SLiteral(8)) in
        let e = make_array (ltype_of_typ (A.PhraseType)) (len) builder in
        ignore(L.build_store e (lookup id) builder); e
      | SSongMeasure(id, idx, phrase) -> let t = A.SongType in 
        build_expr builder (t, SArrAssign(id, idx, phrase))
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
      	      L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
      	      "printf" builder
      | SCall ("printf", [e]) ->
      	      L.build_call printf_func [| float_format_str ; (build_expr builder e) |]
      	      "printf" builder
      | SCall ("prints", [e]) ->
          L.build_call printf_func [| str_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
      | SSongPlay(song) -> 
        let song' = build_expr builder (A.SongType, SId(song)) in
        L.build_call play_song_func [| song' |] "play_song" builder
          
      (*| SPLay(song) -> 
        let getPhraseObj(song, idx) = build_expr builder (SArrAccess(song, idx)) in
        let getNoteObj(phrase, idx) = build_expr builder (SArrAccess(phrase, idx)) in
        (* BuildList from: https://stackoverflow.com/questions/5653739/building-a-list-of-ints-in-ocaml*)
        let buildList i n =
          let rec aux acc i =
            if i <= n then
              aux (i::acc) (i+1)
            else (List.rev acc)
          in aux [] i
        in 
        let phrases = buildList 0 32 in
        let printPhraseBlockList phrase = 
          let notes = buildList 0 8 in

        phrases = List.map (fun x -> getPhraseObj(song, x)) phrases in*)


        
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIfElse (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb
      | SIf (predicate, then_stmt) ->
      let bool_val = build_expr builder predicate in

      let then_bb = L.append_block context "then" the_function in
      ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);

      let end_bb = L.append_block context "if_end" the_function in
      let build_br_end = L.build_br end_bb in (* partial function *)
      add_terminal (L.builder_at_end context then_bb) build_br_end;

      ignore(L.build_cond_br bool_val then_bb end_bb builder);
      L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb
      | SFor(e1, e2, e3, s) -> build_stmt builder
        ( SBlock [SExpr e1 ; SWhile (e2, SBlock [s ; SExpr e3]) ] ) 
      

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module
