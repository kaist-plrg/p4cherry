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

exception CompileError of string
exception V1Model of string

let rec compile_type' (typ: T.typ): C.ctyp = match typ with
  | T.StructT (name, _) -> C.CTStruct name
  | T.HeaderT (name, _) -> C.CTStruct name
  | T.BoolT -> C.CTBool
  | T.ExternT (name, _)-> (match name with
    | "packet_in" -> C.CTStruct "packet_in"
    | "packet_out" -> C.CTStruct "packet_out"
    | _ -> raise (CompileError ("Unsupported extern type: " ^ name)))
  | T.FBitT width -> if (Bigint.to_int_exn width) <= 32 then C.CTUInt else C.CTULInt
  | T.FIntT width -> if (Bigint.to_int_exn width) <= 32 then C.CTInt else C.CTLInt
  | _ -> failwith "Not implemented"

and compile_type (typ: Il.typ): C.ctyp = compile_type' (S.it typ)

(* TODO: Add header types and link it with main function *)
and compile_decl_from_type (typ: Il.typ): C.cdecl = match S.it typ with
  | T.StructT (name, fields) -> C.CDStruct (name, 
  List.map (fun ((fname, ftyp): string * T.typ) -> (compile_type' ftyp, fname)) fields)
  | _ -> raise (CompileError "Expected a struct type for type parameters in instantiation")

and compile_params (params: Il.param list): C.cparam list = List.map (fun param ->
  let (id, _, typ, _, _) = S.it param in
  let ctyp = compile_type typ in
  (C.CTPointer ctyp, S.it id)
) params

and compile_locals (locals: Il.decl list): C.cstmt list = 
  let decls = List.map compile_decl locals in
  List.map (fun x -> C.CSDecl x) decls

and compile_var (var: Il.var): C.cexpr = C.CEVar (get_var_name var)

and compile_z (z: V.t): C.cexpr = match z with
| V.IntV _ | V.FIntV _ | V.FBitV _ as v -> C.CEInt (V.get_num v |> Bigint.to_int_exn)
| _ -> raise (CompileError "Expected a number value")

and compile_value (value: Il.value): C.cexpr = match S.it value with
  | V.BoolV b -> C.CEBool b
  | V.IntV _ | V.FIntV _ | V.FBitV _ as v -> compile_z v
  | _ -> failwith "Not implemented"

and compile_expr (ctx: Ctx.t) (expr: Il.expr): C.cexpr = 
  match S.it expr with
  | Il.ValueE {value;} -> compile_value value
  | Il.VarE {var;} -> let cvar = compile_var var in if Ctx.mem_params ctx (get_var_name var) then C.CEUniExpr (C.CUDereference, cvar) else cvar
  | Il.ExprAccE {expr_base; member;} -> C.CEMember (compile_expr ctx expr_base, S.it member)
  | _ -> failwith "Not implemented"

and compile_statement (ctx: Ctx.t) (stmt: Il.stmt): C.cstmt list = match S.it stmt with
  | L.TransS {expr_label;} -> (match S.it expr_label with
    | Il.VarE {var;} -> [C.CSGoto (get_var_name var)]
    | _ -> raise (CompileError "Expected a variable expression"))
  | L.AssignS {expr_l; expr_r;} -> [C.CSAssign (compile_expr ctx expr_l, compile_expr ctx expr_r)]
  | _ -> failwith "Not implemented"

and compile_state (ctx: Ctx.t) (state: Il.parser_state): C.cstmt list = 
  let (state_label, block, _) = S.it state in
  let c_label = C.CSLabel (S.it state_label) in
  let stmts, _ = S.it block in
  let c_block = List.fold_right (fun stmt acc -> compile_statement ctx stmt @ acc) stmts [] in
  c_label :: c_block

and compile_states (ctx: Ctx.t) (states: Il.parser_state list) = List.fold_left (fun acc state -> acc @ compile_state ctx state) [] states

and compile_parser
  (id: Il.id)
  (tparams: Il.tparam list)
  (params: Il.param list)
  (cparams: Il.cparam list)
  (locals: Il.decl list) 
  (states: Il.parser_state list)
  (annos: Il.anno list) : C.cdecl =
  if tparams <> [] then raise (CompileError "Type parameters are not supported")
  else if cparams <> [] then raise (CompileError "Constructor parameters are not supported")
  else if annos <> [] then raise (CompileError "Annotations are not supported")
  else
    let ctx = Ctx.empty in
    let fname = id.it in
    let compiled_params = compile_params params in
    let ctx = List.fold_left (fun ctx param -> Ctx.add_params ctx param) ctx compiled_params in
    let compiled_locals = compile_locals locals in
    let compiled_states = compile_states ctx states in
    C.CDFunction (C.CTVoid, fname, compiled_params, compiled_locals @ compiled_states @ [C.CSLabel "accept"; C.CSLabel "reject"; C.CSReturn None])

and compile_control
  (id: Il.id)
  (tparams: Il.tparam list)
  (params: Il.param list)
  (cparams: Il.cparam list)
  (locals: Il.decl list) 
  (body: Il.block)
  (annos: Il.anno list) : C.cdecl =
  if tparams <> [] then raise (CompileError "Type parameters are not supported")
  else if cparams <> [] then raise (CompileError "Constructor parameters are not supported")
  else if annos <> [] then raise (CompileError "Annotations are not supported")
  else
    let ctx = Ctx.empty in
    let fname = id.it in
    let compiled_params = compile_params params in
    let ctx = List.fold_left (fun ctx param -> Ctx.add_params ctx param) ctx compiled_params in
    let compiled_locals = compile_locals locals in
    let (stmts, _) = S.it body in
    let compiled_body = List.fold_left (fun acc stmt -> acc @ compile_statement ctx stmt) [] stmts in
    C.CDFunction (C.CTVoid, fname, compiled_params, compiled_locals @ compiled_body)

and compile_decl' (decl: Il.decl'): C.cdecl = 
  match decl with
  | ParserD {id; tparams; params; cparams; locals; states; annos} -> compile_parser id tparams params cparams locals states annos
  | ControlD {id; tparams; params; cparams; locals; body; annos} -> compile_control id tparams params cparams locals body annos
  | _ -> failwith "Not implemented"
  
and compile_decl (decl: Il.decl): C.cdecl = compile_decl' (S.it decl)

and generate_main_fn (inst_decl: Il.decl) = 
  let header_type_name = type_name (List.nth (fetch_targs inst_decl) 0) in
  let metadata_type_name = type_name (List.nth (fetch_targs inst_decl) 1) in
  let standard_metadata_type_name = "standard_metadata_t" in
  let parser_name = fetch_parser_inst_name inst_decl in
  let control_names = fetch_control_inst_names inst_decl in
  C.CDFunction (
  C.CTInt, 
  "main", 
  [], 
  [ C.CSDecl (C.CDVar (C.CTStruct "packet_in", "packet_in"))
  ; C.CSDecl (C.CDVar (C.CTStruct "packet_out", "packet_out"))
  ; C.CSExpr (C.CECall (("init_packet_in"), [C.CEUniExpr (C.CUReference, C.CEVar "packet_in")]))
  ; C.CSExpr (C.CECall (("init_packet_out"), [C.CEUniExpr (C.CUReference, C.CEVar "packet_out")]))
  ; C.CSDecl (C.CDVar (C.CTStruct header_type_name, "h"))
  ; C.CSDecl (C.CDVar (C.CTStruct metadata_type_name, "m"))
  ; C.CSDecl (C.CDVar (C.CTStruct standard_metadata_type_name, "sm"))
  ; C.CSExpr (C.CECall (parser_name,
      [ C.CEUniExpr (C.CUReference, C.CEVar "packet_in")
      ; C.CEUniExpr (C.CUReference, C.CEVar "h")
      ; C.CEUniExpr (C.CUReference, C.CEVar "m")
      ; C.CEUniExpr (C.CUReference, C.CEVar "sm")]))
  ; C.CSExpr (C.CECall (List.nth control_names 0,
      [ C.CEUniExpr (C.CUReference, C.CEVar "h")
      ; C.CEUniExpr (C.CUReference, C.CEVar "m")]))
  ; C.CSExpr (C.CECall (List.nth control_names 1,
      [ C.CEUniExpr (C.CUReference, C.CEVar "h")
      ; C.CEUniExpr (C.CUReference, C.CEVar "m")
      ; C.CEUniExpr (C.CUReference, C.CEVar "sm")]))
  ; C.CSExpr (C.CECall (List.nth control_names 2,
      [ C.CEUniExpr (C.CUReference, C.CEVar "h")
      ; C.CEUniExpr (C.CUReference, C.CEVar "m")
      ; C.CEUniExpr (C.CUReference, C.CEVar "sm")]))
  ; C.CSExpr (C.CECall (List.nth control_names 3,
      [ C.CEUniExpr (C.CUReference, C.CEVar "h")
      ; C.CEUniExpr (C.CUReference, C.CEVar "m")]))
  ; C.CSExpr (C.CECall (List.nth control_names 4,
      [ C.CEUniExpr (C.CUReference, C.CEVar "packet_out")
      ; C.CEUniExpr (C.CUReference, C.CEVar "h")]))
  ; C.CSReturn (Some (C.CEInt 0))
  ])

and compile (program: Il.program) = 
  let c_includes = ["#include <cluj.h>"; "#include <stdbool.h>";] in
  let il_inst_d = fetch_instantiate program "V1Switch" in
  let il_targs = fetch_targs il_inst_d in
  let c_targs = List.map compile_struct_decl_from_type il_targs in
  let il_parser = fetch_parser il_inst_d program in
  let c_parser = compile_decl il_parser in
  let il_controls = fetch_control il_inst_d program in
  let c_controls = List.map compile_decl il_controls in
  Pp.pp_program Format.std_formatter (C.CProgram (c_includes, c_targs @ [c_parser] @ c_controls @ [generate_main_fn il_inst_d]))

(* Note
- maybe the things to be added to main should be passed around in context and updated since that is the orchestration function.
 *)