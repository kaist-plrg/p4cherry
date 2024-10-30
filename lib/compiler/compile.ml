module Pp = C.Pp
module C = C.Syntax
module Il = Il.Ast
module S = Util.Source
module L = Lang.Ast
module T = Runtime.Types
module V = Runtime.Value

(* module H = Helper *)
open Option
open Helper

(* Assumptions:
   - As a general rule, all function parameters will be passed by reference. The input program is type-checked so read-only `in` values cannot appear as l-values, etc. so we don't have to worry about things like writing to read-only values. `out` and `inout` values are passed by reference anyway.
*)

exception CompileError of string
exception V1Model of string

let rec compile_type' (typ : T.typ) : C.ctyp =
  match typ with
  | T.StructT (name, _) -> C.CTStruct name
  | T.HeaderT (name, _) -> C.CTStruct name
  | T.BoolT -> C.CTBool
  | T.ExternT (name, _) -> (
      match name with
      | "packet_in" -> C.CTStruct "packet_in"
      | "packet_out" -> C.CTStruct "packet_out"
      | _ -> raise (CompileError ("Unsupported extern type: " ^ name)))
  | T.FBitT width ->
      if Bigint.to_int_exn width <= 32 then C.CTUInt else C.CTULInt
  | T.FIntT width -> if Bigint.to_int_exn width <= 32 then C.CTInt else C.CTLInt
  | _ -> failwith "Not implemented"

and compile_type (typ : Il.typ) : C.ctyp = compile_type' (S.it typ)

(* Given input type, returns compiled type and compiled declarations to be added in the global namespace. Eg. structs, headers, etc. *)
and compile_decl_from_type (typ : T.typ) : C.ctyp * C.cdecl list =
  match typ with
  | T.StructT (name, fields) as x ->
      ( compile_type' x,
        let typ_list, decl_list =
          List.fold_left
            (fun acc (fname, ftyp) ->
              let ctyp, cdecls = compile_decl_from_type ftyp in
              let typ_list, decl_list = acc in
              (typ_list @ [ (ctyp, fname) ], decl_list @ cdecls))
            ([], []) fields
        in
        C.CDStruct (name, typ_list) :: decl_list )
  | T.HeaderT (name, fields) as x ->
      ( compile_type' x,
        let typ_list, decl_list =
          List.fold_left
            (fun acc (fname, ftyp) ->
              let ctyp, cdecls = compile_decl_from_type ftyp in
              let typ_list, decl_list = acc in
              (typ_list @ [ (ctyp, fname) ], decl_list @ cdecls))
            ([], []) fields
        in
        C.CDStruct (name, typ_list) :: decl_list )
      (* TODO: the header declaration must precede the struct declaration. Also, add an isValid field: This is complicated-memcpy relies on size of struct and isValid increases the size *)
      (* isValid must be set to true on packet.extract *)
  | _ -> (compile_type' typ, [])

and compile_decls_from_type (typ_list : Il.typ list) :
    C.ctyp list * C.cdecl list =
  List.fold_left
    (fun acc typ ->
      let ctyp, cdecls = compile_decl_from_type (S.it typ) in
      let typ_list, decl_list = acc in
      (typ_list @ [ ctyp ], decl_list @ cdecls))
    ([], []) typ_list

and compile_params (params : Il.param list) : C.cparam list =
  List.map
    (fun param ->
      let id, dir, typ, _, _ = S.it param in
      let ctyp = compile_type typ in
      match S.it dir with
      | L.In | L.InOut | L.Out -> (C.CTPointer ctyp, S.it id)
      | L.No -> (C.CTPointer ctyp, S.it id))
    params

and compile_parser_local (local : Il.decl) : C.cstmt =
  C.CSDecl (compile_decl local)

and compile_table (ctx : Ctx.t) (prefix : string) (id : Il.id)
    (keys : Il.table_key list) (actions : Il.table_action list)
    (entries : Il.table_entry list) (default : Il.table_default option)
    (customs : Il.table_custom list) : C.cdecl =
  failwith "Not implemented"

and compile_control_local (ctx : Ctx.t) (prefix : string) (local : Il.decl) :
    C.cdecl =
  match S.it local with
  | ActionD { id; params; body; _ } ->
      let compiled_params = compile_params params in
      let ctx = Ctx.add_params ctx compiled_params in
      C.CDFunction
        ( C.CTVoid,
          prefix ^ "_" ^ id.it,
          List.rev (Ctx.get_params ctx),
          List.fold_left
            (fun acc stmt -> acc @ compile_statement ctx stmt)
            []
            (fst (S.it body)) )
  | TableD { id; table; _ } ->
      let { L.keys; L.actions; L.entries; L.default; L.customs } = table in
      compile_table ctx prefix id keys actions entries default customs
  | _ -> failwith "Not implemented"

and compile_var (var : Il.var) : C.cexpr = C.CEVar (get_var_name var)

and compile_z (z : V.t) : C.cexpr =
  match z with
  | (V.IntV _ | V.FIntV _ | V.FBitV _) as v ->
      C.CEInt (V.get_num v |> Bigint.to_int_exn)
  | _ -> raise (CompileError "Expected a number value")

and compile_value (value : Il.value) : C.cexpr =
  match S.it value with
  | V.BoolV b -> C.CEBool b
  | (V.IntV _ | V.FIntV _ | V.FBitV _) as v -> compile_z v
  | _ -> failwith "Not implemented"

and compile_binop (op : Il.binop) : C.bop =
  match S.it op with
  | PlusOp -> C.CBAdd
  | SPlusOp -> C.CBAdd
  | MinusOp -> C.CBSub
  | SMinusOp -> C.CBSub
  | MulOp -> C.CBMul
  | DivOp -> C.CBDiv
  | ModOp -> C.CBMod
  | ShlOp -> C.CBShl
  | ShrOp -> C.CBShr
  | LeOp -> C.CBLte
  | GeOp -> C.CBGte
  | LtOp -> C.CBLt
  | GtOp -> C.CBGt
  | EqOp -> C.CBEq
  | NeOp -> C.CBNe
  | BAndOp -> C.CBAnd
  | BXorOp -> C.CBXor
  | BOrOp -> C.CBOr
  | ConcatOp -> raise (CompileError "ConcatOp not supported")
  | LAndOp -> C.CBLAnd
  | LOrOp -> C.CBLOr

and compile_expr (ctx : Ctx.t) (expr : Il.expr) : C.cexpr =
  match S.it expr with
  | Il.ValueE { value } -> compile_value value
  | Il.CastE { typ; expr } -> C.CECast (compile_type typ, compile_expr ctx expr)
  | Il.VarE { var } ->
      let cvar = compile_var var in
      if Ctx.mem_param ctx (get_var_name var) then
        C.CEUniExpr (C.CUDereference, cvar)
        (* All function parameters are pointer variables to comply with in, inout, out semantics of parameters. However, P4 programs have no notion of addresses. Only values. So within a block, pointers must always be dereferenced before use. *)
      else cvar
  | Il.ExprAccE { expr_base; member } ->
      C.CEMember (compile_expr ctx expr_base, S.it member)
  | Il.BinE { binop; expr_l; expr_r } ->
      C.CECompExpr
        (compile_binop binop, compile_expr ctx expr_l, compile_expr ctx expr_r)
  | _ -> failwith "Not implemented"

and compile_arg_helper (ctx : Ctx.t) (arg : Il.arg) : C.cstmt option * C.cexpr =
  match S.it arg with
  | L.ExprA x ->
      if is_lval x then (None, compile_expr ctx x)
      else
        let { Il.typ; _ } = S.note x in
        let temp_name = create_temporary () in
        ( Some
            (C.CSDecl
               (C.CDVar (compile_type' typ, temp_name, Some (compile_expr ctx x)))),
          C.CEVar temp_name )
  | L.NameA (id, expr) -> raise @@ CompileError "Named Arguments not supported"
  | L.AnyA -> failwith "Not implemented"

and compile_arg (ctx : Ctx.t) (arg : Il.arg) : C.cstmt option * C.cexpr =
  let stmt, expr = compile_arg_helper ctx arg in
  (stmt, C.CEUniExpr (C.CUAddressOf, expr))

and compile_arg_without_address_of_op (ctx : Ctx.t) (arg : Il.arg) :
    C.cstmt option * C.cexpr =
  compile_arg_helper ctx arg

and compile_args (ctx : Ctx.t) (args : Il.arg list) :
    C.cstmt list * C.cexpr list =
  List.fold_left
    (fun (stmts, exprs) arg ->
      match arg with
      | Some x, expr -> (stmts @ [ x ], exprs @ [ expr ])
      | None, expr -> (stmts, exprs @ [ expr ]))
    ([], [])
    (List.map (compile_arg ctx) args)

and compile_special_extern_function (ctx : Ctx.t) (expr_base : Il.expr)
    (member : Il.member) (args : Il.arg list) : C.cstmt list =
  let extern_typ =
    get_extern_type_name (expr_base |> S.note |> get_type_from_note)
  in
  let cexpr_base = C.CEUniExpr (C.CUAddressOf, compile_expr ctx expr_base) in
  let c_member = S.it member in
  let c_temp_stmts, cargs = compile_args ctx args in
  let cargs_without_address_of_op =
    List.map (fun arg -> snd (compile_arg_without_address_of_op ctx arg)) args
  in
  match extern_typ with
  | "packet_in" -> (
      match c_member with
      | "extract" ->
          if List.length cargs <> 1 then
            raise
            @@ CompileError "packet_in.extract(...) has more than 1 argument"
          else
            [
              C.CSExpr
                (C.CECall
                   ( C.CEVar c_member,
                     (cexpr_base :: cargs)
                     @ [
                         C.CECall (C.CEVar "sizeof", cargs_without_address_of_op);
                       ] ));
            ]
      | "advance" ->
          if List.length cargs <> 1 then
            raise
            @@ CompileError "packet_in.extract(...) has more than 1 argument"
          else [ C.CSExpr (C.CECall (C.CEVar c_member, cexpr_base :: cargs)) ]
      | _ ->
          raise
          @@ CompileError
               ("Unexpected member function called on packet_in object:"
              ^ c_member))
  | "packet_out" -> (
      match c_member with
      | "emit" ->
          if List.length cargs <> 1 then
            raise
            @@ CompileError "packet_out.emit(...) has more than 1 argument"
          else
            [
              C.CSExpr
                (C.CECall
                   ( C.CEVar "extract",
                     (cexpr_base :: cargs)
                     @ [ C.CECall (C.CEVar "sizeof", cargs) ] ));
            ]
      | _ ->
          raise
          @@ CompileError
               ("Unexpected member function called on packet_out object:"
              ^ c_member))
  | _ -> failwith "Not implemented"

and compile_function_call (ctx : Ctx.t) (var_func : Il.var) (args : Il.arg list)
    : C.cstmt list =
  let fname = get_var_name var_func in
  match fname with
  | _ ->
      let c_temp_stmts, cargs = compile_args ctx args in
      c_temp_stmts @ [ C.CSExpr (C.CECall (compile_var var_func, cargs)) ]

(* Assumption: method call will always invoke an extern object as expr_base *)
and compile_method_call (ctx : Ctx.t) (expr_base : Il.expr) (member : Il.member)
    (args : Il.arg list) : C.cstmt list =
  if is_extern_type (expr_base |> S.note |> get_type_from_note) then
    let extern_name =
      get_extern_type_name (expr_base |> S.note |> get_type_from_note)
    in
    match extern_name with
    | "packet_in" | "packet_out" ->
        compile_special_extern_function ctx expr_base member args
    | _ -> failwith "Not implemented"
  else
    let cexpr_base = compile_expr ctx expr_base in
    let c_member = S.it member in
    let c_temp_stmts, cargs =
      List.fold_left
        (fun (stmts, exprs) arg ->
          match arg with
          | Some x, expr -> (stmts @ [ x ], exprs @ [ expr ])
          | None, expr -> (stmts, exprs @ [ expr ]))
        ([], [])
        (List.map (compile_arg ctx) args)
    in
    c_temp_stmts
    @ [ C.CSExpr (C.CECall (C.CEMember (cexpr_base, c_member), cargs)) ]

and compile_select_cases (ctx : Ctx.t) (c_switched_expr : C.cexpr)
    (select_case_lst : Il.select_case list) : C.cstmt list =
  match select_case_lst with
  | [] -> []
  | { it; _ } :: rest -> (
      let keyset_lst, state_label = it in
      let c_state_label = S.it state_label in
      match keyset_lst with
      | [] -> raise (CompileError "Empty keyset list in select case statement")
      | [ keyset ] -> (
          match S.it keyset with
          | L.DefaultK -> [ C.CSGoto c_state_label ]
          | L.AnyK ->
              [
                C.CSIf
                  ( C.CEBool true,
                    [ C.CSGoto c_state_label ],
                    compile_select_cases ctx c_switched_expr rest );
              ]
          | L.ExprK x ->
              [
                C.CSIf
                  ( C.CECompExpr (C.CBEq, c_switched_expr, compile_expr ctx x),
                    [ C.CSGoto c_state_label ],
                    compile_select_cases ctx c_switched_expr rest );
              ])
      | _ ->
          raise
            (CompileError
               "Multiple keysets in select case statement not supported"))

and compile_statement (ctx : Ctx.t) (stmt : Il.stmt) : C.cstmt list =
  match S.it stmt with
  | L.TransS { expr_label } -> (
      match S.it expr_label with
      | Il.VarE { var } -> [ C.CSGoto (get_var_name var) ]
      | Il.SelectE { exprs_select; cases } ->
          if List.length exprs_select > 1 then
            raise
              (CompileError
                 "Select statement with multiple expressions not supported")
          else
            let expr = List.hd exprs_select in
            let c_switched_expr = compile_expr ctx expr in
            let cstmts = compile_select_cases ctx c_switched_expr cases in
            cstmts
      | _ -> raise (CompileError "Expected a variable or select expression"))
  | L.AssignS { expr_l; expr_r } ->
      [ C.CSAssign (compile_expr ctx expr_l, compile_expr ctx expr_r) ]
  | L.CallMethodS { expr_base; member; args } ->
      compile_method_call ctx expr_base member args
  | L.CallFuncS { var_func; args; _ } -> compile_function_call ctx var_func args
  | _ -> failwith "Not implemented"

and compile_state (ctx : Ctx.t) (state : Il.parser_state) : C.cstmt list =
  let state_label, block, _ = S.it state in
  let c_label = C.CSLabel (S.it state_label) in
  let stmts, _ = S.it block in
  let c_block =
    List.fold_right (fun stmt acc -> compile_statement ctx stmt @ acc) stmts []
  in
  c_label :: [ C.CSBlock c_block ]

and compile_states (ctx : Ctx.t) (states : Il.parser_state list) =
  List.fold_left (fun acc state -> acc @ compile_state ctx state) [] states

and compile_parser (id : Il.id) (tparams : Il.tparam list)
    (params : Il.param list) (cparams : Il.cparam list) (locals : Il.decl list)
    (states : Il.parser_state list) (annos : Il.anno list) : C.cdecl =
  if tparams <> [] then raise (CompileError "Type parameters are not supported")
  else if cparams <> [] then
    raise (CompileError "Constructor parameters are not supported")
  else if annos <> [] then raise (CompileError "Annotations are not supported")
  else
    let ctx = Ctx.empty in
    let fname = id.it in
    let compiled_params = compile_params params in
    let ctx = Ctx.add_params ctx compiled_params in
    let compiled_locals = List.map compile_parser_local locals in
    let compiled_states = compile_states ctx states in
    C.CDFunction
      ( C.CTVoid,
        fname,
        compiled_params,
        compiled_locals @ compiled_states
        @ [
            C.CSLabel "reject";
            C.CSExpr
              (C.CECall
                 ( C.CEVar "mark_to_drop",
                   [
                     C.CEUniExpr
                       ( C.CUAddressOf,
                         C.CEUniExpr
                           ( C.CUDereference,
                             C.CEVar (snd (List.nth compiled_params 3)) ) );
                   ] ));
            C.CSLabel "accept";
            C.CSReturn None;
          ] )

and compile_control (id : Il.id) (tparams : Il.tparam list)
    (params : Il.param list) (cparams : Il.cparam list) (locals : Il.decl list)
    (body : Il.block) (annos : Il.anno list) : C.cdecl list =
  if tparams <> [] then raise (CompileError "Type parameters are not supported")
  else if cparams <> [] then
    raise (CompileError "Constructor parameters are not supported")
  else if annos <> [] then raise (CompileError "Annotations are not supported")
  else
    let ctx = Ctx.empty in
    let fname = id.it in
    let compiled_params = compile_params params in
    let ctx = Ctx.add_params ctx compiled_params in
    let compiled_locals = List.map (compile_control_local ctx fname) locals in
    let stmts, _ = S.it body in
    let compiled_body =
      List.fold_left (fun acc stmt -> acc @ compile_statement ctx stmt) [] stmts
    in
    []

and compile_decl' (decl : Il.decl') : C.cdecl =
  match decl with
  | ParserD { id; tparams; params; cparams; locals; states; annos } ->
      compile_parser id tparams params cparams locals states annos
  | ControlD { id; tparams; params; cparams; locals; body; annos } ->
      compile_control id tparams params cparams locals body annos
  | ConstD { id; typ; value; annos } ->
      let ctyp = compile_type typ in
      let cvalue = compile_value value in
      C.CDVar (ctyp, id.it, Some cvalue)
  | _ -> failwith "Not implemented"

and compile_decl (decl : Il.decl) : C.cdecl = compile_decl' (S.it decl)

and generate_main_fn (inst_decl : Il.decl) =
  let header_type_name = type_name (List.nth (fetch_targs inst_decl) 0) in
  let metadata_type_name = type_name (List.nth (fetch_targs inst_decl) 1) in
  let standard_metadata_type_name = "standard_metadata_t" in
  let parser_name = fetch_parser_inst_name inst_decl in
  let control_names = fetch_control_inst_names inst_decl in
  C.CDFunction
    ( C.CTInt,
      "main",
      [],
      [
        C.CSDecl (C.CDVar (C.CTStruct "packet_in", "packet_in", None));
        C.CSExpr
          (C.CECall
             ( C.CEVar "init_packet_in",
               [ C.CEUniExpr (C.CUAddressOf, C.CEVar "packet_in") ] ));
        C.CSDecl (C.CDVar (C.CTStruct "packet_out", "packet_out", None));
        C.CSExpr
          (C.CECall
             ( C.CEVar "init_packet_out",
               [ C.CEUniExpr (C.CUAddressOf, C.CEVar "packet_out") ] ));
        C.CSDecl (C.CDVar (C.CTStruct header_type_name, "h", None));
        C.CSDecl (C.CDVar (C.CTStruct metadata_type_name, "m", None));
        C.CSDecl (C.CDVar (C.CTStruct standard_metadata_type_name, "sm", None));
        C.CSExpr
          (C.CECall
             ( C.CEVar parser_name,
               [
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "packet_in");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "h");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "m");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "sm");
               ] ));
        C.CSExpr
          (C.CECall
             ( C.CEVar (List.nth control_names 0),
               [
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "h");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "m");
               ] ));
        C.CSExpr
          (C.CECall
             ( C.CEVar (List.nth control_names 1),
               [
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "h");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "m");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "sm");
               ] ));
        C.CSExpr
          (C.CECall
             ( C.CEVar (List.nth control_names 2),
               [
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "h");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "m");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "sm");
               ] ));
        C.CSExpr
          (C.CECall
             ( C.CEVar (List.nth control_names 3),
               [
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "h");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "m");
               ] ));
        C.CSExpr
          (C.CECall
             ( C.CEVar (List.nth control_names 4),
               [
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "packet_out");
                 C.CEUniExpr (C.CUAddressOf, C.CEVar "h");
               ] ));
        C.CSReturn (Some (C.CEInt 0));
      ] )

and compile (program : Il.program) =
  let c_includes = [ "#include <stdbool.h>"; "#include \"cluj_core.h\"" ] in
  (* TODO: process all declarations in order instead of fetching from instantiate *)
  let il_inst_d = fetch_instantiate program "V1Switch" in
  (* TODO: Change instantiation method invocations and append apply at the end. Example: MyParser() -> MyParser_apply()*)
  let il_targs = fetch_targs il_inst_d in
  let c_targs_decls = snd (compile_decls_from_type il_targs) in
  let il_parser = fetch_parser il_inst_d program in
  let c_parser = compile_decl il_parser in
  Pp.pp_program Format.std_formatter (C.CProgram ([], [ c_parser ]))
(* let il_controls = fetch_control il_inst_d program in
   let c_controls = List.map compile_decl il_controls in
   let c_program =
     C.CProgram
       ( c_includes,
         c_targs_decls @ [ c_parser ] @ c_controls
         @ [ generate_main_fn il_inst_d ] )
   in
   Pp.pp_program Format.std_formatter c_program *)
(* program *)
