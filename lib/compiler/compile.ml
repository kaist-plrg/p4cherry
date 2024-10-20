module C = C.Syntax
module Il = Il.Ast
module S = Util.Source
module L = Lang.Ast

exception CompileError of string

let rec placeholder () = failwith "Not implemented"
and compile_params ctx (params: Il.param list) : Ctx.t * C.cparam list = failwith "Not implemented"
  (* match params with
  | [] -> ctx, []
  | hd :: tl -> 
    let (id, _, typ, v, _) = hd.it in failwith "Not implemented" *)

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
  if tparams <> [] then raise (CompileError "Type parameters are not supported")
  else if cparams <> [] then raise (CompileError "Constructor parameters are not supported")
  else if annos <> [] then raise (CompileError "Annotations are not supported")
  else
    let fname = id.it in
    let ctx, compiled_params = compile_params ctx params in
    let ctx, compiled_locals = compile_locals ctx locals in
    let ctx, compiled_states = compile_states ctx states in
    (ctx, C.CDFunction (C.CTVoid, fname, compiled_params, compiled_locals @ compiled_states @ [C.CSLabel "accept"; C.CSLabel "reject"; C.CSReturn None]))

and compile_decl (ctx: Ctx.t) (decl: Il.decl'): Ctx.t * C.cdecl = 
  match decl with
  | ParserD {id; tparams; params; cparams; locals; states; annos} -> compile_parser ctx id tparams params cparams locals states annos
  | _ -> ctx, CDEmpty
  

(* and compile_decl (ctx: Ctx.t) (decl: Il.decl): Ctx.t * C.cdecl =
  let it = decl.it in 
  compile_decl' ctx it *)

and compile_decls (ctx: Ctx.t) (decls: Il.decl list): Ctx.t * C.cdecl list = 
  List.fold_right (fun (decl: Il.decl) (ctx, cdecls) -> 
    let ctx', cdecl = compile_decl ctx decl.it in
    (ctx', cdecl :: cdecls)
  ) decls (ctx, [])

(* let fetch_and_compile_parser (program: Il.program): C.cdecl = 
  let  *)

and fetch_instantiate (program: Il.program) (arch: string): Il.decl = 
  List.find (fun (decl: Il.decl' S.phrase) -> 
  match S.it decl with 
  | Il.InstD {var_inst = y;} -> (match S.it y with L.Current x | L.Top x -> S.it x = arch)
  | _ -> false)
  program

(* fetch_parser_inst will fetch the name of the parser component
  instantiated in the main instantiation declaration. This name
  can be used to fetch the actual parser declarations *)
and fetch_parser_inst_name (program: Il.program) : string = 
  let inst_decl = fetch_instantiate program "V1Switch" in
  let args = fetch_args inst_decl in
  let parser_arg_opt = List.find_opt (fun arg ->
    match S.it arg with
    | L.ExprA x -> (
      match (S.note x : Il.note) with 
      | {typ = ParserT _;} -> true 
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
  | L.ExprA {it;} -> (
        match it with 
        | Il.InstE {var_inst;} -> (match S.it var_inst with 
          | L.Current x | L.Top x -> S.it x)
        | _ -> raise (CompileError "Expected an instantiation expression")
  )
  | _ -> raise (CompileError "Expected an expression argument")

(* fetch_control_insts will fetch the names of the control components 
  instantiated in the main instantiation declaration. These names
  can be used to fetch the actual control declarations *)
and fetch_control_inst_names (program: Il.program) : string list = 
  let inst_decl = fetch_instantiate program "V1Switch" in
  let args = fetch_args inst_decl in
  let filtered_args = List.filter (fun arg ->
    match S.it arg with
    | L.ExprA x -> (
      match (S.note x : Il.note) with 
      | {typ = ControlT _;} -> true 
      | _ -> false
      )
    | _ -> false
    ) args
  in
  let () = if (List.length filtered_args <> 5) then raise (CompileError "Expected 5 control components in V1Switch instantiation") in
  List.map (fun arg -> 
    match S.it arg with 
    | L.ExprA {it;} -> (
        match it with 
        | Il.InstE {var_inst;} -> (match S.it var_inst with 
          | L.Current x | L.Top x -> S.it x)
        | _ -> raise (CompileError "Expected an instantiation expression")
    )
    | _ -> raise (CompileError "Expected an expression argument")
  ) filtered_args

and fetch_parser (program: Il.program): Il.decl = 
  let parser_name = fetch_parser_inst_name program in
  List.find (fun (decl: Il.decl' S.phrase) -> 
  match S.it decl with 
  | Il.ParserD {id; _} -> S.it id = parser_name
  | _ -> false)
  program

and fetch_control (program: Il.program): Il.decl list =
  let control_names = fetch_control_inst_names program in
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
  
and fetch_args (inst_decl: Il.decl) : Il.arg list = 
  match S.it inst_decl with 
  | Il.InstD {args; _} -> args
  | _ -> raise (CompileError "Expected an instantiation declaration")

and compile (program: Il.program) = fetch_control program

(* Note
- maybe the things to be added to main should be passed around in context and updated since that is the orchestration function.
 *)