open Syntax
open Ast
open Runtime
open Utils

(* Environments *)

type env = Env.t
type tenv = Tenv.t
type ccenv = Ccenv.t
type store = Store.t

(* Utils *)

let rec name_from_type (typ : Type.t) : string =
  match typ with
  | TypeName { name = BareName text; _ } -> text.str
  (* (TODO) how to consider type arguments? *)
  | SpecializedType { base; _ } -> name_from_type base
  | _ ->
      Printf.sprintf "(name_from_type) Unexpected type: %s"
        (Pretty.print_type typ)
      |> failwith

(* Instantiation *)

(* Instantiating a declaration *)

(* Loading the result of a declaration
   (other than instantiation) to env, tenv, or ccenv *)

let rec load (env : env) (tenv : tenv) (ccenv : ccenv) (decl : Declaration.t) :
    env * tenv * ccenv =
  match decl with
  (* Loading constructor closures to ccenv *)
  | PackageType { name; params; type_params; _ } ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cclos = Cclosure.Package { params; tparams } in
      let ccenv = Ccenv.insert name cclos ccenv in
      (env, tenv, ccenv)
  | Parser { name; params; type_params; constructor_params; locals; states; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        Cclosure.Parser { params; tparams; cparams; locals; states }
      in
      let ccenv = Ccenv.insert name cclos ccenv in
      (env, tenv, ccenv)
  | Control { name; params; type_params; constructor_params; locals; apply; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        Cclosure.Control { params; tparams; cparams; locals; apply }
      in
      let ccenv = Ccenv.insert name cclos ccenv in
      (env, tenv, ccenv)
  | ExternObject { name; _ } ->
      let name = name.str in
      let cclos = Cclosure.Extern in
      let ccenv = Ccenv.insert name cclos ccenv in
      (env, tenv, ccenv)
  (* Loading types to tenv *)
  (* (TODO) assume type declarations are at top level only *)
  | TypeDef { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Static.eval_typ env tenv typ in
          let tenv = Tenv.insert name typ tenv in
          (env, tenv, ccenv)
      | Alternative.Right _decl ->
          Printf.eprintf "(TODO: load) Loading typedef with decl %s\n" name;
          (env, tenv, ccenv))
  | NewType { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Static.eval_typ env tenv typ in
          let tenv = Tenv.insert name typ tenv in
          (env, tenv, ccenv)
      | Alternative.Right _decl ->
          Printf.eprintf "(TODO: load) Loading newtype with decl %s\n" name;
          (env, tenv, ccenv))
  | Enum { name; members; _ } ->
      let name = name.str in
      let entries = List.map (fun (member : Text.t) -> member.str) members in
      let typ = Typ.Enum { entries } in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, ccenv)
  | SerializableEnum { name; typ; members; _ } ->
      let name = name.str in
      let typ = Static.eval_typ env tenv typ in
      let entries =
        List.map
          (fun member ->
            let member : Text.t = fst member in
            member.str)
          members
      in
      let typ = Typ.SEnum { typ; entries } in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, ccenv)
  | Header { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ env tenv field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Header { entries } in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, ccenv)
  | HeaderUnion { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ env tenv field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Union { entries } in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, ccenv)
  | Struct { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ env tenv field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Struct { entries } in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, ccenv)
  (* Loading constants to env *)
  | Function { name; _ } ->
      Printf.eprintf "(TODO: load) Loading function %s\n" name.str;
      (env, tenv, ccenv)
  (* Loading constants *)
  (* (TODO) consider implicit casts? *)
  | Constant { name; typ; value; _ } ->
      let typ = Static.eval_typ env tenv typ in
      let value = Static.eval_expr env tenv value in
      let value = Ops.eval_cast typ value in
      let env = Env.insert name.str value env in
      (env, tenv, ccenv)
  | _ -> (env, tenv, ccenv)

(* Evaluating instantiation arguments,
   which may contain nameless instantiation *)

and eval_targs (env : env) (tenv : tenv) (tparams : string list)
    (typs : Type.t list) : tenv =
  print_endline "Evaluating type arguments";
  String.concat ", " tparams |> print_endline;
  String.concat ", " (List.map (fun typ -> Pretty.print_type typ) typs)
  |> print_endline;
  assert (List.length tparams = List.length typs);
  List.fold_left2
    (fun tenv tparam typ ->
      let typ = Static.eval_typ env tenv typ in
      Tenv.insert tparam typ tenv)
    tenv tparams typs

and eval_expr (env : env) (tenv : tenv) (ccenv : ccenv) (store : store)
    (path : string list) (expr : Expression.t) : Value.t * store =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      let store = instantiate_expr env tenv ccenv store path typ args in
      let value = Value.Ref path in
      (value, store)
  | _ ->
      let value = Static.eval_expr env tenv expr in
      (value, store)

and eval_args (env : env) (tenv : tenv) (ccenv : ccenv) (store : store)
    (path : string list) (params : Parameter.t list) (args : Argument.t list) :
    env * store =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  (* It is illegal to use names only for some arguments:
     either all or no arguments must specify the parameter name. (8.20) *)
  assert (
    List.for_all
      (fun (arg : Argument.t) ->
        match arg with Expression _ -> true | _ -> false)
      args
    || List.for_all
         (fun (arg : Argument.t) ->
           match arg with KeyValue _ -> true | _ -> false)
         args);
  let params, args =
    List.fold_left2
      (fun (params, args) (param : Parameter.t) (arg : Argument.t) ->
        match arg with
        | Expression { value; _ } ->
            (params @ [ param.variable.str ], args @ [ value ])
        | KeyValue { key; value; _ } -> (params @ [ key.str ], args @ [ value ])
        | _ ->
            failwith "(eval_args) Instantiation argument must not be missing.")
      ([], []) params args
  in
  List.fold_left2
    (fun (env', store) param arg ->
      let value, store =
        eval_expr env tenv ccenv store (path @ [ param ]) arg
      in
      let env' = Env.insert param value env' in
      (env', store))
    (env, store) params args

(* Instantiation of a constructor closure *)

and instantiate_cclos (env : env) (tenv : tenv) (ccenv : ccenv) (store : store)
    (path : string list) (cclos : Cclosure.t) (args : Argument.t list)
    (_typs : Type.t list) : store =
  match cclos with
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  (* Every time a parser is instantiated, it causes:
     every extern and parser instantiation that is in the parser source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclosure.Parser { params; tparams = _tparams; cparams; locals; states } ->
      let env_parser = Env.enter env in
      let tenv_parser = Tenv.enter tenv in
      let ccenv_parser = Ccenv.enter ccenv in
      (* let tenv_parser = eval_targs env tenv_parser tparams typs in *)
      let env_parser, store =
        eval_args env_parser tenv_parser ccenv_parser store path cparams args
      in
      let env_parser, tenv_parser, ccenv_parser, store =
        List.fold_left
          (fun (env, tenv, ccenv, store) local ->
            instantiate_decl env tenv ccenv store path local)
          (env_parser, tenv_parser, ccenv_parser, store)
          locals
      in
      let stmts =
        List.concat_map (fun (state : Parser.state) -> state.statements) states
      in
      let env_parser, _ccenv_parser, store =
        List.fold_left
          (fun (env, ccenv, store) stmt ->
            instantiate_stmt env tenv ccenv store path stmt)
          (env_parser, ccenv_parser, store)
          stmts
      in
      let obj =
        Object.Parser
          { env = env_parser; tenv = tenv_parser; params; locals; states }
      in
      let store = Store.insert path obj store in
      store
  (* Every time a control is instantiated, it causes:
     1 instantiation of each table defined within it
     every extern and control instantiation that is in the control source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclosure.Control { params; tparams = _tparams; cparams; locals; apply; _ }
    ->
      let env_control = Env.enter env in
      let tenv_control = Tenv.enter tenv in
      let ccenv_control = Ccenv.enter ccenv in
      (* let tenv_control = eval_targs env tenv_control tparams typs in *)
      let env_control, store =
        eval_args env_control tenv_control ccenv_control store path cparams args
      in
      let env_control, tenv_control, ccenv_control, store =
        List.fold_left
          (fun (env, tenv, ccenv, store) local ->
            instantiate_decl env tenv ccenv store path local)
          (env_control, tenv_control, ccenv_control, store)
          locals
      in
      let stmts = apply.statements in
      let env_control, _ccenv_control, store =
        List.fold_left
          (fun (env, ccenv, store) stmt ->
            instantiate_stmt env tenv ccenv store path stmt)
          (env_control, ccenv_control, store)
          stmts
      in
      let obj =
        Object.Control
          { env = env_control; tenv = tenv_control; params; locals; apply }
      in
      let store = Store.insert path obj store in
      store
  (* Others do not involve recursive instantiation other than the args *)
  | Cclosure.Package { params; tparams = _tparams } ->
      let env_package = Env.enter env in
      let tenv_package = Tenv.enter tenv in
      (* let tenv_package = eval_targs env tenv_package tparams typs in *)
      let env_package, store =
        eval_args env_package tenv_package ccenv store path params args
      in
      let obj = Object.Package { env = env_package; tenv = tenv_package } in
      let store = Store.insert path obj store in
      store
  | Cclosure.Extern ->
      let obj = Object.Extern in
      let store = Store.insert path obj store in
      store

and instantiate_expr (env : env) (tenv : tenv) (ccenv : ccenv) (store : store)
    (path : string list) (typ : Type.t) (args : Argument.t list) : store =
  let cclos, targs = Ccenv.find_from_type typ ccenv in
  let store = instantiate_cclos env tenv ccenv store path cclos args targs in
  store

and instantiate_stmt (env : env) (tenv : tenv) (ccenv : ccenv) (store : store)
    (path : string list) (stmt : Statement.t) : env * ccenv * store =
  match stmt with
  | BlockStatement { block; _ } ->
      let stmts = block.statements in
      List.fold_left
        (fun (env, ccenv, store) stmt ->
          instantiate_stmt env tenv ccenv store path stmt)
        (env, ccenv, store) stmts
  | DirectApplication { typ; args; _ } ->
      let name = name_from_type typ in
      let cclos, targs = Ccenv.find_from_type typ ccenv in
      let store =
        instantiate_cclos env tenv ccenv store (path @ [ name ]) cclos args
          targs
      in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, ccenv, store)
  | _ -> (env, ccenv, store)

and instantiate_decl (env : env) (tenv : tenv) (ccenv : ccenv) (store : store)
    (path : string list) (decl : Declaration.t) : env * tenv * ccenv * store =
  match decl with
  (* Explicit instantiation *)
  | Instantiation { name; typ; args; _ } ->
      let name = name.str in
      let cclos, targs = Ccenv.find_from_type typ ccenv in
      let store =
        instantiate_cclos env tenv ccenv store (path @ [ name ]) cclos args
          targs
      in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, tenv, ccenv, store)
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; properties; _ } ->
      let name = name.str in
      let obj = Object.Table { properties } in
      let store = Store.insert (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, tenv, ccenv, store)
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let name = name.str in
      let obj = Object.ValueSet in
      let store = Store.insert (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, tenv, ccenv, store)
  (* Load declarations to either env or ccenv *)
  | _ ->
      let env, tenv, ccenv = load env tenv ccenv decl in
      (env, tenv, ccenv, store)

let instantiate_program (program : program) =
  let (Program decls) = program in
  let env = Env.empty in
  let tenv = Tenv.empty in
  let ccenv = Ccenv.empty in
  let store = Store.empty in
  let _, _, _, store =
    List.fold_left
      (fun (env, tenv, ccenv, store) decl ->
        instantiate_decl env tenv ccenv store [] decl)
      (env, tenv, ccenv, store) decls
  in
  store
