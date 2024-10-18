module C = C.Syntax
module Il = Il.Ast

let rec placeholder = failwith "Not implemented"
and compile_params ctx params = failwith "Not implemented"

and compile_locals ctx locals = 
  let ctx, decls = compile_decls ctx locals in 
  ctx, List.map (fun x -> C.CSDecl x) decls

and compile_states ctx states = failwith "Not implemented"

and compile_parser
  (ctx: Ctx.t)
  (id: Il.id)
  (tparams: Il.tparam list)
  (params: Il.param list)
  (cparams: Il.cparam list)
  (locals: Il.decl list) 
  (states: Il.parser_state list)
  (annos: Il.anno list) : Ctx.t * C.cdecl =
  let fname = id.it in
  let ctx, compiled_params = compile_params ctx params in
  let ctx, compiled_locals = compile_locals ctx locals in
  let ctx, compiled_states = compile_states ctx states in
  (ctx, C.CDFunction (C.CTVoid, fname, compiled_params, compiled_locals @ compiled_states @ [C.CSLabel "accept"; C.CSLabel "reject"; C.CSReturn None]))

and compile_decl' (ctx: Ctx.t) (decl: Il.decl'): Ctx.t * C.cdecl = 
  match decl with
  | ParserD {id; tparams; params; cparams; locals; states; annos} -> compile_parser ctx id tparams params cparams locals states annos
  | _ -> ctx, CDEmpty
  

and compile_decl (ctx: Ctx.t) (decl: Il.decl): Ctx.t * C.cdecl =
  let it = decl.it in 
  compile_decl' ctx it

and compile_decls (ctx: Ctx.t) (decls: Il.decl list): Ctx.t * C.cdecl list = 
  List.fold_left (fun (ctx, cdecls) decl -> 
    let ctx', cdecl = compile_decl ctx decl in
    (ctx', cdecl :: cdecls)
  ) (ctx, []) decls

and compile (program: Il.program): C.cprog = 
  let ctx, prog = compile_decls Ctx.empty program in
  CProgram ([], prog)