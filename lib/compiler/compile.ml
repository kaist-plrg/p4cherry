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

type action_info = { action_name : string; action_data_type_name : string }

type cparameters = {
  pointer_params : C.cparam list;
  value_params : C.cparam list;
  all_params_in_order : C.cparam list;
}

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
        decl_list @ [ C.CDStruct (name, typ_list) ] )
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
        decl_list @ [ C.CDStruct (name, typ_list @ [ (C.CTBool, "isValid") ]) ]
      )
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

and compile_params (params : Il.param list) : cparameters =
  let compiled_params =
    List.map
      (fun param ->
        let id, dir, typ, _, _ = S.it param in
        let ctyp = compile_type typ in
        match S.it dir with
        | L.In | L.InOut | L.Out -> (C.CTPointer ctyp, S.it id)
        | L.No -> (
            match ctyp with
            | C.CTStruct "packet_in" -> (C.CTPointer ctyp, S.it id)
            | C.CTStruct "packet_out" -> (C.CTPointer ctyp, S.it id)
            | _ -> (ctyp, S.it id)))
      params
  in
  {
    pointer_params =
      List.filter
        (fun (ctyp, _) -> match ctyp with C.CTPointer _ -> true | _ -> false)
        compiled_params;
    value_params =
      List.filter
        (fun (ctyp, _) -> match ctyp with C.CTPointer _ -> false | _ -> true)
        compiled_params;
    all_params_in_order = compiled_params;
  }

and compile_parser_local (local : Il.decl) : C.cstmt =
  match compile_decl local with
  | hd :: [] -> C.CSDecl hd
  | _ -> failwith "Expected a single declaration"

(* Returns corresponding struct field name *)
and generate_key (_ : Ctx.t) (_ : string) (key : Il.table_key) : C.ctyp * string
    =
  let expr, match_kind, _ = S.it key in
  let cexpr = compile_expr Ctx.empty expr in
  let typ = compile_type' (get_type_from_note (S.note expr)) in
  (* ctx is made empty. Otherwise, compile_expr will dereference variables and struct fields can't have dereferenced variables in the declaration *)
  match S.it match_kind with
  | "exact" ->
      let expr_string = Format.asprintf "%a" Pp.pp_expr cexpr in
      let expr_string =
        String.map (fun c -> if c = '.' then '_' else c) expr_string
      in
      (typ, expr_string)
  | m -> raise (CompileError ("Unsupported match kind: " ^ m))

and compile_keys (ctx : Ctx.t) (prefix : string) (keys : Il.table_key list) :
    C.cdecl =
  C.CDStruct (prefix ^ "_key_t", List.map (generate_key ctx prefix) keys)

and generate_action_enum (_ : Ctx.t) (prefix : string)
    (actions : Il.table_action list) : C.cdecl =
  let action_enum_name = prefix ^ "_action_type_t" in
  let action_enum_members =
    List.map
      (fun action ->
        let var, _, _ = S.it action in
        "ACTION_" ^ get_var_name var)
      actions
  in
  C.CDEnum (action_enum_name, action_enum_members)

and generate_action_data_fields (ctx : Ctx.t) (action_name : string) :
    (C.ctyp * C.cvar) list =
  match Ctx.get_action_signature ctx action_name with
  | Some { action_data; _ } ->
      List.map
        (fun (ctyp, field_name) ->
          match ctyp with
          | C.CTPointer ctyp -> (C.CTPointer ctyp, field_name)
          | ctyp -> (ctyp, field_name))
        action_data
  | None -> (
      match action_name with
      | "NoAction" -> []
      | _ -> raise (CompileError ("Unknown action: " ^ action_name)))

and generate_action_data (ctx : Ctx.t) (prefix : string)
    (action : Il.table_action) : C.cdecl =
  (* 1. create struct with name prefix_[action_name]_data_t
     2. Check the ctx for the action signature. If not there, test if it is NoAction
     3. If ctx has it, retrieve the name of the field and remove the CTPointer from the type to create the field in the struct
     4. If ctx does not have it and it is NoAction, return empty struct
  *)
  let action_name =
    get_var_name
      (let var, _, _ = S.it action in
       var)
  in
  let action_data_name = prefix ^ "_" ^ action_name ^ "_data_t" in
  let action_data_fields = generate_action_data_fields ctx action_name in
  C.CDStruct (action_data_name, action_data_fields)

and generate_action_data_union (_ : Ctx.t) (prefix : string)
    (action_info : action_info list) : C.cdecl =
  let action_data_union_name = prefix ^ "_action_data_t" in
  let action_data_union_fields =
    List.map
      (fun { action_name; action_data_type_name } ->
        (C.CTStruct action_data_type_name, action_name))
      action_info
  in
  C.CDUnion (action_data_union_name, action_data_union_fields)

and generate_action_entry_type (_ : Ctx.t) (prefix : string) : C.cdecl =
  let action_entry_type_name = prefix ^ "_action_t" in
  let action_entry_type_members =
    [
      (C.CTEnum (prefix ^ "_action_type_t"), "action");
      (C.CTUnion (prefix ^ "_action_data_t"), "data");
    ]
  in
  C.CDStruct (action_entry_type_name, action_entry_type_members)

and compile_table_entry (_ : Ctx.t) (_ : string) (_ : Il.table_entry) : C.cdecl
    =
  failwith "Not implemented"

and compile_actions (ctx : Ctx.t) (prefix : string)
    (actions : Il.table_action list) : C.cdecl list =
  (* Structs to declare:
      [x] action type with all the actions (enum)
      [x] action data for each action (struct)
      [x] union type for all action data types
      - action type combining action enum type and action data union type *)
  let action_types_enum = generate_action_enum ctx prefix actions in
  let action_data_structs =
    List.map (generate_action_data ctx prefix) actions
  in
  let action_data_union =
    generate_action_data_union ctx prefix
      (List.map
         (fun action ->
           let var, _, _ = S.it action in
           {
             action_name = get_var_name var;
             action_data_type_name = prefix ^ "_" ^ get_var_name var ^ "_data_t";
           })
         actions)
  in
  let action_entry_struct = generate_action_entry_type ctx prefix in
  [ action_types_enum ] @ action_data_structs
  @ [ action_data_union; action_entry_struct ]

and generate_table_entry (_ : Ctx.t) (prefix : string) : C.cdecl =
  C.CDStruct
    ( prefix ^ "_entry_t",
      [
        (C.CTStruct (prefix ^ "_key_t"), "key");
        (C.CTStruct (prefix ^ "_action_t"), "action");
      ] )

and generate_table_size_const (ctx : Ctx.t) (prefix : string)
    (customs : Il.table_custom list) : C.cdecl =
  let lst =
    List.map
      (fun custom ->
        match S.it custom with
        | member, value, _, _ -> (
            match S.it member with
            | "size" ->
                C.CDVar
                  ( C.CTConst C.CTLInt,
                    prefix ^ "_table_size",
                    Some (compile_expr ctx value) )
            | x ->
                raise (CompileError ("Unexpected custom table property: " ^ x))))
      customs
  in
  if List.length lst = 0 then
    raise
      (CompileError
         "Table size not specified. Since tables are statically allocated, \
          size must be specified")
  else List.hd lst

and generate_table_array (_ : Ctx.t) (prefix : string) (size_expr : C.cexpr) :
    C.cdecl =
  C.CDArray
    (C.CTArray (C.CTStruct (prefix ^ "_entry_t")), prefix ^ "_table", size_expr)

and compile_table_default_action (ctx : Ctx.t) (prefix : string)
    (default : Il.table_default option) : C.cdecl =
  let default_action_name = prefix ^ "_default_action" in
  match default with
  | Some v ->
      let table_action, _ = S.it v in
      let action_name, args, _ = S.it table_action in
      let action_name = get_var_name action_name in
      let action_data = C.CEStruct (List.map (compile_arg ctx) args) in
      (* TODO: Add locals in scope to ctx *)
      C.CDVar
        ( C.CTStruct (prefix ^ "_action_t"),
          default_action_name,
          Some (C.CEStruct [ C.CEVar ("ACTION_" ^ action_name); action_data ])
        )
  | None ->
      C.CDVar
        ( C.CTStruct (prefix ^ "_action_t"),
          default_action_name,
          Some (C.CEStruct [ C.CEVar "ACTION_NoAction"; C.CEStruct [] ]) )

and generate_table_action_result (_ : Ctx.t) (prefix : string) : C.cdecl =
  C.CDStruct
    ( prefix ^ "_apply_result_t",
      [ (C.CTBool, "hit"); (C.CTStruct (prefix ^ "_action_t"), "action_run") ]
    )

and generate_table_apply_function (ctx : Ctx.t) (control_prefix : string)
    (table_prefix : string) (keys : Il.table_key list)
    (actions : Il.table_action list) : C.cdecl =
  let prefix = control_prefix ^ "_" ^ table_prefix in
  let apply_result_type = C.CTStruct (prefix ^ "_apply_result_t") in
  let table_entry_type = C.CTStruct (prefix ^ "_entry_t") in
  let apply_result_var = "result" in
  let key_type = C.CTStruct (prefix ^ "_key_t") in
  let lookup_key_var = "lookup_key" in
  let default_action_var = prefix ^ "_default_action" in
  let table_size_var = prefix ^ "_table_size" in
  let table_entry_var = "table_entry" in
  let build_key_stmts =
    List.map
      (fun key ->
        let expr, _, _ = S.it key in
        C.CSAssign
          ( C.CEMember
              (C.CEVar lookup_key_var, snd (generate_key ctx prefix key)),
            compile_expr ctx expr ))
      keys
  in
  let key_field_names =
    List.map (fun key -> snd (generate_key ctx prefix key)) keys
  in
  let gen_equality_predicate key =
    C.CECompExpr
      ( C.CBEq,
        C.CEMember (C.CEVar lookup_key_var, key),
        C.CEMember (C.CEMember (C.CEVar table_entry_var, "key"), key) )
  in
  let lookup_equality_predicate =
    List.fold_left
      (fun acc x -> C.CECompExpr (C.CBLogicalAnd, acc, x))
      (C.CEBool true)
      (List.map gen_equality_predicate key_field_names)
  in
  let switch_case_stmts =
    List.map
      (fun action ->
        let action_name, _, _ = S.it action in
        let action_name = get_var_name action_name in
        if action_name = "NoAction" then
          (C.CEVar "ACTION_NoAction", [ C.CSBreak ])
        else
          let case_label = "ACTION_" ^ action_name in
          let action_fname =
            match Ctx.get_action_signature ctx action_name with
            | Some { compiled_function_name; _ } -> compiled_function_name
            | None ->
                raise (CompileError ("Action not defined: " ^ action_name))
          in
          let lambda_lifted_arguments =
            List.map (fun param -> C.CEVar (snd param)) (Ctx.get_params ctx)
          in
          let action_data_fields =
            generate_action_data_fields ctx action_name
          in
          let action_data_params =
            List.map
              (fun (typ, name) ->
                let temp =
                  C.CEMember
                    ( C.CEMember
                        ( C.CEMember
                            ( C.CEMember (C.CEVar apply_result_var, "action_run"),
                              "data" ),
                          action_name ),
                      name )
                in
                match typ with
                | C.CTPointer _ -> C.CEUniExpr (C.CUAddressOf, temp)
                | _ -> temp)
              action_data_fields
          in
          ( C.CEVar case_label,
            [
              C.CSExpr
                (C.CECall
                   ( C.CEVar action_fname,
                     lambda_lifted_arguments @ action_data_params ));
              C.CSBreak;
            ] ))
      actions
  in
  let func =
    C.CDFunction
      ( apply_result_type,
        prefix ^ "_apply",
        Ctx.get_params ctx,
        [
          C.CSDecl (C.CDVar (apply_result_type, apply_result_var, None));
          (* Build Key *)
          C.CSDecl (C.CDVar (key_type, lookup_key_var, None));
        ]
        @ build_key_stmts
        @ (* Perform lookup *)
        [
          C.CSAssign
            (C.CEMember (C.CEVar apply_result_var, "hit"), C.CEBool false);
          C.CSAssign
            ( C.CEMember (C.CEVar apply_result_var, "action_run"),
              C.CEVar default_action_var );
          C.CSDecl (C.CDVar (C.CTInt, "i", Some (C.CEInt 0)));
          C.CSWhile
            ( C.CECompExpr (C.CBLt, C.CEVar "i", C.CEVar table_size_var),
              [
                C.CSDecl
                  (C.CDVar
                     ( table_entry_type,
                       table_entry_var,
                       Some
                         (C.CEArrayAccess
                            (C.CEVar (prefix ^ "_table"), C.CEVar "i")) ));
                C.CSIf
                  ( lookup_equality_predicate,
                    [
                      C.CSAssign
                        ( C.CEMember (C.CEVar apply_result_var, "hit"),
                          C.CEBool true );
                      C.CSAssign
                        ( C.CEMember (C.CEVar apply_result_var, "action_run"),
                          C.CEMember (C.CEVar table_entry_var, "action") );
                      C.CSBreak;
                    ],
                    [] );
                C.CSAssign
                  (C.CEVar "i", C.CECompExpr (C.CBAdd, C.CEVar "i", C.CEInt 1));
              ] );
          (* Execute Action *)
          C.CSSwitch
            ( C.CEMember
                (C.CEMember (C.CEVar apply_result_var, "action_run"), "action"),
              switch_case_stmts,
              None );
          C.CSReturn (Some (C.CEVar apply_result_var));
        ] )
  in
  func

and compile_table (ctx : Ctx.t) (id : Il.id) (keys : Il.table_key list)
    (actions : Il.table_action list) (entries : Il.table_entry list)
    (default : Il.table_default option) (customs : Il.table_custom list) :
    C.cdecl list =
  let prefix = Ctx.get_prefix_string ctx ^ "_" ^ id.it in
  let key_struct = compile_keys ctx prefix keys in
  let action_decls = compile_actions ctx prefix actions in
  let _ = List.map (compile_table_entry ctx prefix) entries in
  let table_entry_struct = generate_table_entry ctx prefix in
  let table_size_decl = generate_table_size_const ctx prefix customs in
  let table_array_decl =
    generate_table_array ctx prefix (C.CEVar (prefix ^ "_table_size"))
  in
  let table_default_action = compile_table_default_action ctx prefix default in
  let table_action_result = generate_table_action_result ctx prefix in
  let table_apply_function =
    generate_table_apply_function ctx
      (Ctx.get_prefix_string ctx)
      id.it keys actions
  in
  [ key_struct ] @ action_decls
  @ [
      table_entry_struct;
      table_size_decl;
      table_array_decl;
      table_default_action;
      table_action_result;
      table_apply_function;
    ]

and compile_control_local (ctx : Ctx.t) (local : Il.decl) : Ctx.t * C.cdecl list
    =
  match S.it local with
  | ActionD { id; params; body; _ } ->
      let { pointer_params; all_params_in_order; _ } = compile_params params in
      let compiled_params = all_params_in_order in
      let compiled_fname = Ctx.get_prefix_string ctx ^ "_" ^ id.it in
      let ctx2 = Ctx.add_params ctx pointer_params in
      let action_sig =
        Ctx.
          {
            action_name = id.it;
            action_data = compiled_params;
            compiled_function_name = compiled_fname;
          }
      in
      let ctx_out = Ctx.add_action_signature ctx2 action_sig in
      let action_func =
        C.CDFunction
          ( C.CTVoid,
            compiled_fname,
            Ctx.get_params ctx @ compiled_params,
            List.fold_left
              (fun acc stmt -> acc @ compile_statement ctx2 stmt)
              []
              (fst (S.it body)) )
      in
      (ctx_out, [ action_func ])
  | TableD { id; table; _ } ->
      let { L.keys; L.actions; L.entries; L.default; L.customs } = table in
      (ctx, compile_table ctx id keys actions entries default customs)
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
  | LAndOp -> C.CBLogicalAnd
  | LOrOp -> C.CBLogicalOr

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
  | Il.CallMethodE { expr_base; member; args; _ } ->
      compile_method_call ctx expr_base member args
  | _ -> failwith "Not implemented"

and compile_arg (ctx : Ctx.t) (arg : Il.arg) : C.cexpr =
  match S.it arg with
  | L.ExprA x ->
      let { Il.typ; _ } = S.note x in
      if is_lval x then compile_expr ctx x
      else C.CECompoundLiteral (compile_type' typ, [ compile_expr ctx x ])
  | L.NameA _ -> raise @@ CompileError "Named Arguments not supported"
  | L.AnyA -> failwith "Not implemented"

and compile_arg_as_pointer (ctx : Ctx.t) (arg : Il.arg) : C.cexpr =
  let expr = compile_arg ctx arg in
  C.CEUniExpr (C.CUAddressOf, expr)

and compile_args_as_pointers (ctx : Ctx.t) (args : Il.arg list) : C.cexpr list =
  List.fold_left
    (fun exprs arg -> exprs @ [ compile_arg_as_pointer ctx arg ])
    [] args

and compile_special_extern_function (ctx : Ctx.t) (expr_base : Il.expr)
    (member : Il.member) (args : Il.arg list) : C.cexpr =
  let extern_typ =
    get_extern_type_name (expr_base |> S.note |> get_type_from_note)
  in
  let cexpr_base = C.CEUniExpr (C.CUAddressOf, compile_expr ctx expr_base) in
  let c_member = S.it member in
  let carg_pointers = compile_args_as_pointers ctx args in
  let cargs = List.map (compile_arg ctx) args in
  match extern_typ with
  | "packet_in" -> (
      match c_member with
      | "extract" ->
          if List.length carg_pointers <> 1 then
            raise
            @@ CompileError "packet_in.extract(...) has more than 1 argument"
          else
            C.CECall
              ( C.CEVar c_member,
                [ cexpr_base ] @ carg_pointers
                @ [
                    C.CECompExpr
                      ( C.CBSub,
                        C.CECall (C.CEVar "sizeof", cargs),
                        C.CECall (C.CEVar "sizeof", [ C.CEVar "bool" ]) );
                  ] )
      | "advance" ->
          if List.length carg_pointers <> 1 then
            raise
            @@ CompileError "packet_in.extract(...) has more than 1 argument"
          else C.CECall (C.CEVar c_member, cexpr_base :: carg_pointers)
      | _ ->
          raise
          @@ CompileError
               ("Unexpected member function called on packet_in object:"
              ^ c_member))
  | "packet_out" -> (
      match c_member with
      | "emit" ->
          if List.length carg_pointers <> 1 then
            raise
            @@ CompileError "packet_out.emit(...) has more than 1 argument"
          else
            C.CECall
              ( C.CEVar c_member,
                (cexpr_base :: carg_pointers)
                @ [
                    C.CECompExpr
                      ( C.CBSub,
                        C.CECall (C.CEVar "sizeof", cargs),
                        C.CECall (C.CEVar "sizeof", [ C.CEVar "bool" ]) );
                  ] )
      | _ ->
          raise
          @@ CompileError
               ("Unexpected member function called on packet_out object:"
              ^ c_member))
  | _ -> failwith "Not implemented"

and compile_function_call (ctx : Ctx.t) (var_func : Il.var) (args : Il.arg list)
    : C.cexpr =
  let fname = get_var_name var_func in
  match fname with
  | _ ->
      let cargs = compile_args_as_pointers ctx args in
      C.CECall (compile_var var_func, cargs)

(* Assumption: method call will always invoke an extern object as expr_base *)
and compile_method_call (ctx : Ctx.t) (expr_base : Il.expr) (member : Il.member)
    (args : Il.arg list) : C.cexpr =
  let expr_base_type = expr_base |> S.note |> get_type_from_note in
  match expr_base_type with
  | T.ExternT _ -> (
      let extern_name = get_extern_type_name expr_base_type in
      match extern_name with
      | "packet_in" | "packet_out" ->
          compile_special_extern_function ctx expr_base member args
      | _ -> failwith "Not implemented")
  | T.HeaderT _ -> (
      match S.it member with
      | "isValid" -> C.CEMember (compile_expr ctx expr_base, "isValid")
      | _ -> failwith "Not implemented")
  | T.TableT _ -> (
      match S.it member with
      | "apply" -> (
          match S.it expr_base with
          | Il.VarE { var } ->
              let table_name = get_var_name var in
              C.CECall
                ( C.CEVar
                    (Ctx.get_prefix_string ctx ^ "_" ^ table_name ^ "_apply"),
                  List.map (fun x -> C.CEVar (snd x)) (Ctx.get_params ctx) )
          | _ -> failwith "Not implemented")
      | _ -> failwith "Not implemented")
  | _ ->
      let cexpr_base = compile_expr ctx expr_base in
      let c_member = S.it member in
      let cargs = compile_args_as_pointers ctx args in
      C.CECall (C.CEMember (cexpr_base, c_member), cargs)

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
  | L.CallMethodS { expr_base; member; args; _ } ->
      [ C.CSExpr (compile_method_call ctx expr_base member args) ]
  | L.CallFuncS { var_func; args; _ } ->
      [ C.CSExpr (compile_function_call ctx var_func args) ]
  | L.IfS { expr_cond; stmt_then; stmt_else } ->
      let cexpr_cond = compile_expr ctx expr_cond in
      let cstmt_then = compile_statement ctx stmt_then in
      let cstmt_else = compile_statement ctx stmt_else in
      [ C.CSIf (cexpr_cond, cstmt_then, cstmt_else) ]
  | L.BlockS { block } ->
      let stmts, _ = S.it block in
      List.fold_left (fun acc stmt -> acc @ compile_statement ctx stmt) [] stmts
  | L.EmptyS -> [ C.CSSkip ]
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
    let { pointer_params; all_params_in_order; _ } = compile_params params in
    let compiled_params = all_params_in_order in
    let ctx = Ctx.add_params ctx pointer_params in
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
    let ctx = Ctx.set_prefix_string ctx fname in
    let { pointer_params; all_params_in_order; _ } = compile_params params in
    let compiled_params = all_params_in_order in
    let ctx = Ctx.add_params ctx pointer_params in
    let ctx, compiled_locals =
      List.fold_left
        (fun (ctx_in, compiled_locals) local ->
          let ctx_out, compiled_local = compile_control_local ctx_in local in
          (ctx_out, compiled_locals @ compiled_local))
        (ctx, []) locals
    in
    let stmts, _ = S.it body in
    let compiled_body =
      List.fold_left (fun acc stmt -> acc @ compile_statement ctx stmt) [] stmts
    in
    compiled_locals
    @ [ C.CDFunction (C.CTVoid, fname, compiled_params, compiled_body) ]

and compile_decl' (decl : Il.decl') : C.cdecl list =
  match decl with
  | ParserD { id; tparams; params; cparams; locals; states; annos } ->
      [ compile_parser id tparams params cparams locals states annos ]
  | ControlD { id; tparams; params; cparams; locals; body; annos } ->
      compile_control id tparams params cparams locals body annos
  | ConstD { id; typ; value; _ } ->
      let ctyp = compile_type typ in
      let cvalue = compile_value value in
      [ C.CDVar (ctyp, id.it, Some cvalue) ]
  | InstD _ -> []
  | _ -> failwith "Not implemented"

and compile_decl (decl : Il.decl) : C.cdecl list = compile_decl' (S.it decl)

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
  (* let il_parser = fetch_parser il_inst_d program in
     let c_parser = compile_decl il_parser in
     let il_controls = fetch_control il_inst_d program in
     let c_controls =
       List.fold_left
         (fun acc il_control -> acc @ compile_decl il_control)
         [] il_controls *)
  let compiled_decls =
    List.fold_left (fun acc il_decl -> acc @ compile_decl il_decl) [] program
  in
  let c_program =
    C.CProgram
      ( c_includes,
        c_targs_decls @ compiled_decls @ [ generate_main_fn il_inst_d ] )
  in
  Pp.pp_program Format.std_formatter c_program
(* program *)
