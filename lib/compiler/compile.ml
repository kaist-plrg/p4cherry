module Pp = C.Pp
module C = C.Syntax
module Il = Il.Ast
module S = Util.Source
module L = Lang.Ast
module T = Runtime.Types

exception CompileError of string


let rec compile_type' (typ: T.typ): C.ctyp = match typ with
  | T.StructT (name, _) -> C.CTStruct name
  | T.BoolT -> C.CTBool
  | T.ExternT (name, _)-> (match name with
    | "packet_in" -> C.CTUIntBW C.BW16
    | "packet_out" -> C.CTUIntBW C.BW16
    | _ -> raise (CompileError "Unsupported extern type"))
  | _ -> failwith "Not implemented"

and compile_type (typ: Il.typ): C.ctyp = compile_type' (S.it typ)

and compile_struct_decl_from_type (typ: Il.typ): C.cdecl = match S.it typ with
  | T.StructT (name, fields) -> C.CDStruct (name, 
  List.map (fun ((fname, ftyp): string * T.typ) -> (compile_type' ftyp, fname)) fields)
  | _ -> raise (CompileError "Expected a struct type for type parameters in instantiation")

and compile_params (params: Il.param list): C.cparam list = List.map (fun param ->
  let (id, _, typ, _, _) = S.it param in
  let ctyp = compile_type typ in
  (C.CTPointer ctyp, S.it id)
) params

and compile_locals (locals: Il.decl list): C.cstmt list = 
  let decls = compile_decls locals in
  List.map (fun x -> C.CSDecl x) decls

and compile_statement (stmt: Il.stmt): C.cstmt list = match S.it stmt with
  | L.TransS {expr_label;} -> (match S.it expr_label with
    | Il.VarE {var;} -> [C.CSGoto (match S.it var with | L.Current x | L.Top x -> S.it x)]
    | _ -> raise (CompileError "Expected a variable expression"))
  | _ -> failwith "Not implemented"

and compile_state (state: Il.parser_state): C.cstmt list = 
  let (state_label, block, _) = S.it state in
  let c_label = C.CSLabel (S.it state_label) in
  let stmts, _ = S.it block in
  let c_block = List.fold_right (fun stmt acc -> compile_statement stmt @ acc) stmts [] in
  c_label :: c_block

and compile_states (states: Il.parser_state list) = List.fold_right (fun state acc -> compile_state state @ acc) states []

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
    let fname = id.it in
    let compiled_params = compile_params params in
    let compiled_locals = compile_locals locals in
    let compiled_states = compile_states states in
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
    let fname = id.it in
    let compiled_params = compile_params params in
    let compiled_locals = compile_locals locals in
    let (stmts, _) = S.it body in
    let compiled_body = List.fold_right (fun stmt acc -> compile_statement stmt @ acc) stmts [] in
    C.CDFunction (C.CTVoid, fname, compiled_params, compiled_locals @ compiled_body)

and compile_decl (decl: Il.decl'): C.cdecl = 
  match decl with
  | ParserD {id; tparams; params; cparams; locals; states; annos} -> compile_parser id tparams params cparams locals states annos
  | ControlD {id; tparams; params; cparams; locals; body; annos} -> compile_control id tparams params cparams locals body annos
  | _ -> CDEmpty
  

(* and compile_decl (ctx: Ctx.t) (decl: Il.decl): Ctx.t * C.cdecl =
  let it = decl.it in 
  compile_decl' ctx it *)

and compile_decls (decls: Il.decl list): C.cdecl list = 
  List.map compile_decl (List.map S.it decls)

and fetch_instantiate (program: Il.program) (arch: string): Il.decl = 
  List.find (fun (decl: Il.decl' S.phrase) -> 
  match S.it decl with 
  | Il.InstD {var_inst = y; _} -> (match S.it y with L.Current x | L.Top x -> S.it x = arch)
  | _ -> false)
  program

(* fetch_parser_inst will fetch the name of the parser component
  instantiated in the main instantiation declaration. This name
  can be used to fetch the actual parser declarations *)
and fetch_parser_inst_name (inst_decl: Il.decl) : string = 
  let args = fetch_inst_args inst_decl in
  let parser_arg_opt = List.find_opt (fun arg ->
    match S.it arg with
    | L.ExprA x -> (
      match (S.note x : Il.note) with 
      | {typ = ParserT _; _} -> true 
      | _ -> false
      )
    | _ -> false
    ) args
  in
  let parser_arg = match parser_arg_opt with 
    | None -> raise (CompileError "Expected a parser component in V1Switch instantiation")
    | Some x -> x
  in
  match S.it parser_arg with
  | L.ExprA {it; _} -> (
        match it with 
        | Il.InstE {var_inst; _} -> (match S.it var_inst with 
          | L.Current x | L.Top x -> S.it x)
        | _ -> raise (CompileError "Expected an instantiation expression")
  )
  | _ -> raise (CompileError "Expected an expression argument")

(* fetch_control_insts will fetch the names of the control components 
  instantiated in the main instantiation declaration. These names
  can be used to fetch the actual control declarations *)
and fetch_control_inst_names (inst_decl: Il.decl) : string list = 
  let args = fetch_inst_args inst_decl in
  let filtered_args = List.filter (fun arg ->
    match S.it arg with
    | L.ExprA x -> (
      match (S.note x : Il.note) with 
      | {typ = ControlT _; _} -> true 
      | _ -> false
      )
    | _ -> false
    ) args
  in
  let () = if (List.length filtered_args <> 5) then raise (CompileError "Expected 5 control components in V1Switch instantiation") in
  List.map (fun arg -> 
    match S.it arg with 
    | L.ExprA {it; _} -> (
        match it with 
        | Il.InstE {var_inst; _} -> (match S.it var_inst with 
          | L.Current x | L.Top x -> S.it x)
        | _ -> raise (CompileError "Expected an instantiation expression")
    )
    | _ -> raise (CompileError "Expected an expression argument")
  ) filtered_args

and fetch_parser (inst_decl: Il.decl) (program: Il.program): Il.decl = 
  let parser_name = fetch_parser_inst_name inst_decl in
  List.find (fun (decl: Il.decl' S.phrase) -> 
  match S.it decl with 
  | Il.ParserD {id; _} -> S.it id = parser_name
  | _ -> false)
  program

and fetch_control (inst_decl: Il.decl) (program: Il.program): Il.decl list =
  let control_names = fetch_control_inst_names inst_decl in
  List.map (fun control_name -> 
    List.find (fun (decl: Il.decl' S.phrase) -> 
    match S.it decl with 
    | Il.ControlD {id; _} -> S.it id = control_name
    | _ -> false)
    program
  ) control_names

and fetch_targs (inst_decl: Il.decl) : Il.typ list = 
  match S.it inst_decl with 
  | Il.InstD {targs; _} -> targs
  | _ -> raise (CompileError "Expected an instantiation declaration")
  
and fetch_inst_args (inst_decl: Il.decl) : Il.arg list = 
  match S.it inst_decl with 
  | Il.InstD {args; _} -> args
  | _ -> raise (CompileError "Expected an instantiation declaration")

and compile (program: Il.program) = 
  let c_includes = ["#include <cluj.h>"; "#include <stdbool.h>"; "#include <stdint.h>"; "#include <stdlib.h>"] in
  let il_inst_d = fetch_instantiate program "V1Switch" in
  let c_targs = List.map compile_struct_decl_from_type (fetch_targs il_inst_d) in
  let il_parser = fetch_parser il_inst_d program in
  let c_parser = compile_decl (S.it il_parser) in
  let il_controls = fetch_control il_inst_d program in
  (* let c_controls = List.map (fun control -> compile_decl (S.it control)) il_controls in *)
  il_controls
  (* Pp.pp_program Format.std_formatter (C.CProgram (c_includes, c_targs @ [c_parser])) *)

(* Note
- maybe the things to be added to main should be passed around in context and updated since that is the orchestration function.
  - maintain a list of statements for main in Ctx.t
 *)