open Cimpl
module Ast = Il.Ast

let rec placeholder = failwith "Not implemented"
and compile_params ctx params = failwith "Not implemented"

and compile_locals ctx locals = 
  let ctx, decls = compile_decls ctx locals in 
  ctx, List.map (fun x -> CSDecl x) decls

and compile_states ctx states = failwith "Not implemented"

and compile_parser
  (ctx: Ctx.t)
  (id: Ast.id)
  (tparams: Ast.tparam list)
  (params: Ast.param list)
  (cparams: Ast.cparam list)
  (locals: Ast.decl list) 
  (states: Ast.parser_state list)
  (annos: Ast.anno list) : Ctx.t * cdecl =
  let fname = id.it in
  let ctx, compiled_params = compile_params ctx params in
  let ctx, compiled_locals = compile_locals ctx locals in
  let ctx, compiled_states = compile_states ctx states in
  (ctx, CDFunction (CTVoid, fname, compiled_params, compiled_locals @ compiled_states @ [CSLabel "accept"; CSLabel "reject"; CSReturn None]))

and compile_decl' (ctx: Ctx.t) (decl: Ast.decl'): Ctx.t * cdecl = 
  match decl with
  | ParserD {id; tparams; params; cparams; locals; states; annos} -> compile_parser ctx id tparams params cparams locals states annos
  | _ -> failwith "Not implemented"
  

and compile_decl (ctx: Ctx.t) (decl: Ast.decl): Ctx.t * cdecl =
  let it = decl.it in 
  compile_decl' ctx it

and compile_decls (ctx: Ctx.t) (decls: Ast.decl list): Ctx.t * cdecl list = 
  List.fold_left (fun (ctx, cdecls) decl -> 
    let ctx', cdecl = compile_decl ctx decl in
    (ctx', cdecl :: cdecls)
  ) (ctx, []) decls

and compile (program: Ast.program): cprog = 
  let ctx, prog = compile_decls Ctx.empty program in
  CProgram prog