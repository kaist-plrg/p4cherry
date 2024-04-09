open Syntax
open Ast
open Envs
open Runtime.Envs
open Runtime.Store
open Runtime
open Utils

(* Store *)

type store = GSto.t

(* Utils *)

let rec fold_left3 f acc xs ys zs =
  match (xs, ys, zs) with
  | [], [], [] -> acc
  | x :: xs', y :: ys', z :: zs' ->
      let acc' = f acc x y z in
      fold_left3 f acc' xs' ys' zs'
  | _, _, _ -> failwith "(fold_left3) Lists have different lengths"

let rec name_from_type (typ : Type.t) : string =
  match typ with
  | TypeName { name = BareName text; _ } -> text.str
  (* (TODO) how to consider type arguments? *)
  | SpecializedType { base; _ } -> name_from_type base
  | _ ->
      Printf.sprintf "(name_from_type) Unexpected type: %s"
        (Pretty.print_type typ)
      |> failwith

let rec cclos_from_type (typ : Type.t) (ccenv : ccenv) : Cclos.t * Type.t list =
  match typ with
  | Type.TypeName { name = Name.BareName text; _ } ->
      (CCEnv.find text.str ccenv, [])
  | Type.TypeName { name = Name.QualifiedName ([], text); _ } ->
      (CCEnv.find_toplevel text.str ccenv, [])
  | Type.SpecializedType { base; args; _ } ->
      let cclos, _ = cclos_from_type base ccenv in
      (cclos, args)
  | _ -> assert false

(* Instantiation *)

(* Loading the result of a declaration
   (other than instantiation) to constant/type/typdef environment, value/type/object store *)

let rec load_decl (cenv : cenv) (tsto : tsto) (vsto : vsto) (tdenv : tdenv)
    (ccenv : ccenv) (decl : Declaration.t) : cenv * tsto * vsto * tdenv * ccenv
    =
  match decl with
  (* Loading constructor closures to ccenv *)
  (* For package type declaration, also load to tdenv *)
  | PackageType { name; params; type_params; _ } ->
      let name = name.str in
      let typ = Typ.Ref in
      let tdenv = TDEnv.add name typ tdenv in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cclos = Cclos.Package { params; tparams } in
      let ccenv = CCEnv.add name cclos ccenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  | Parser { name; params; type_params; constructor_params; locals; states; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        Cclos.Parser
          { tdenv; cenv; tsto; vsto; params; tparams; cparams; locals; states }
      in
      let ccenv = CCEnv.add name cclos ccenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  | Control { name; params; type_params; constructor_params; locals; apply; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        Cclos.Control
          { tdenv; cenv; tsto; vsto; params; tparams; cparams; locals; apply }
      in
      let ccenv = CCEnv.add name cclos ccenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  (* For package type declaration, also load to tdenv *)
  | ExternObject { name; _ } ->
      let name = name.str in
      let typ = Typ.Ref in
      let tdenv = TDEnv.add name typ tdenv in
      let cclos = Cclos.Extern in
      let ccenv = CCEnv.add name cclos ccenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  (* Loading types to type definition environment *)
  | TypeDef { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Static.eval_typ tdenv cenv vsto typ in
          let tdenv = TDEnv.add name typ tdenv in
          (cenv, tsto, vsto, tdenv, ccenv)
      | Alternative.Right _decl ->
          Printf.eprintf "(TODO: load) Loading typedef with decl %s\n" name;
          (cenv, tsto, vsto, tdenv, ccenv))
  | NewType { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Static.eval_typ tdenv cenv vsto typ in
          let tdenv = TDEnv.add name typ tdenv in
          (cenv, tsto, vsto, tdenv, ccenv)
      | Alternative.Right _decl ->
          Printf.eprintf "(TODO: load) Loading newtype with decl %s\n" name;
          (cenv, tsto, vsto, tdenv, ccenv))
  | Enum { name; members; _ } ->
      let name = name.str in
      let entries = List.map (fun (member : Text.t) -> member.str) members in
      let typ = Typ.Enum { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  | SerializableEnum { name; typ; members; _ } ->
      let name = name.str in
      let typ = Static.eval_typ tdenv cenv vsto typ in
      let entries =
        List.map
          (fun member ->
            let member : Text.t = fst member in
            member.str)
          members
      in
      let typ = Typ.SEnum { typ; entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  | Header { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ tdenv cenv vsto field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Header { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  | HeaderUnion { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ tdenv cenv vsto field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Union { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  | Struct { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ tdenv cenv vsto field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Struct { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  (* (TODO) A better representation of parser/control object types? *)
  | ParserType { name; _ } | ControlType { name; _ } ->
      let name = name.str in
      let typ = Typ.Ref in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tsto, vsto, tdenv, ccenv)
  | Function { name; _ } ->
      Printf.eprintf "(TODO: load) Loading function %s\n" name.str;
      (cenv, tsto, vsto, tdenv, ccenv)
  (* Loading constants to constant environment and value store *)
  | Constant { name; typ; value; _ } ->
      let name = name.str in
      let typ = Static.eval_typ tdenv cenv vsto typ in
      let value = Static.eval_expr tdenv cenv vsto value |> Ops.eval_cast typ in
      let cenv, tsto, vsto = load_const cenv tsto vsto name typ value in
      (cenv, tsto, vsto, tdenv, ccenv)
  | _ -> (cenv, tsto, vsto, tdenv, ccenv)

(* Evaluating instantiation arguments,
   which may contain nameless instantiation *)

(*
and eval_targs (cenv : cenv) (tdenv : tdenv) (tparams : string list)
    (typs : Type.t list) : tdenv =
  print_endline "Evaluating type arguments";
  String.concat ", " tparams |> print_endline;
  String.concat ", " (List.map (fun typ -> Pretty.print_type typ) typs)
  |> print_endline;
  assert (List.length tparams = List.length typs);
  List.fold_left2
    (fun tdenv tparam typ ->
      let typ = Static.eval_typ cenv vsto tdenv typ in
      TDEnv.add tparam typ tdenv)
    tdenv tparams typs
*)

and eval_expr (cenv : cenv) (tsto : tsto) (vsto : vsto) (tdenv : tdenv)
    (ccenv : ccenv) (store : store) (path : string list) (expr : Expression.t) :
    Value.t * store =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      let store =
        instantiate_expr cenv tsto vsto tdenv ccenv store path typ args
      in
      let value = Value.Ref path in
      (value, store)
  | _ ->
      let value = Static.eval_expr tdenv cenv vsto expr in
      (value, store)

and eval_args (cenv : cenv) (tsto : tsto) (vsto : vsto) (tdenv : tdenv)
    (ccenv : ccenv) (store : store) (path : string list)
    (params : Parameter.t list) (args : Argument.t list) :
    string list * Typ.t list * Value.t list * store =
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
            (params @ [ (param.variable.str, param.typ) ], args @ [ value ])
        | KeyValue { key; value; _ } ->
            (params @ [ (key.str, param.typ) ], args @ [ value ])
        | _ ->
            failwith "(eval_args) Instantiation argument must not be missing.")
      ([], []) params args
  in
  List.fold_left2
    (fun (names, typs, values, store) param arg ->
      let param, typ = param in
      let typ = Static.eval_typ tdenv cenv vsto typ in
      let value, store =
        eval_expr cenv tsto vsto tdenv ccenv store (path @ [ param ]) arg
      in
      (names @ [ param ], typs @ [ typ ], values @ [ value ], store))
    ([], [], [], store) params args

(* Instantiation of a constructor closure *)

and instantiate_cclos (cenv : cenv) (tsto : tsto) (vsto : vsto) (tdenv : tdenv)
    (ccenv : ccenv) (store : store) (path : string list) (cclos : Cclos.t)
    (args : Argument.t list) (_typs : Type.t list) : store =
  let var_decl_to_stmt (decl : Declaration.t) : Statement.t option =
    match decl with
    | Variable { tags; _ } -> Some (DeclarationStatement { tags; decl })
    | _ -> None 
  in
  match cclos with
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  (* Every time a parser is instantiated, it causes:
     every extern and parser instantiation that is in the parser source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclos.Parser
      {
        tdenv = tdenv_local;
        cenv = cenv_local;
        tsto = tsto_local;
        vsto = vsto_local;
        params;
        tparams = _tparams;
        cparams;
        locals;
        states;
      } ->
      let cenv_local = CEnv.enter cenv_local in
      let tsto_local = TSto.enter tsto_local in
      let vsto_local = VSto.enter vsto_local in
      let lenv_local = LEnv.empty in
      (* Add constructor arguments to constant environment and value store *)
      (* let tdenv_local = eval_targs cenv tdenv_local tparams typs in *)
      let names, typs, values, store =
        eval_args cenv tsto vsto tdenv ccenv store path cparams args
      in
      let cenv_local, tsto_local, vsto_local =
        fold_left3
          (fun (cenv, tsto, vsto) name typ value ->
            load_const cenv tsto vsto name typ value)
          (cenv_local, tsto_local, vsto_local)
          names typs values
      in
      (* Instantiate local instantiations, load constants and local variables *)
      let cenv_local, lenv_local, tsto_local, vsto_local, store =
        List.fold_left
          (fun (cenv, lenv, tsto, vsto, store) local ->
            instantiate_parser_local_decl cenv lenv tsto vsto tdenv_local ccenv
              store path local)
          (cenv_local, lenv_local, tsto_local, vsto_local, store)
          locals
      in
      (* Build methods out of states *)
      let funcs_state, store =
        List.fold_left
          (fun ((funcs, store) : Func.t list * store) (state : Parser.state) ->
            let name = state.name.str in
            let body = state.statements in
            let transition = state.transition in
            let cenv_local, _tsto_local, _vsto_local, store =
              List.fold_left
                (fun (cenv, tsto, vsto, store) stmt ->
                  instantiate_stmt cenv tsto vsto tdenv ccenv store path stmt)
                (cenv_local, tsto_local, vsto_local, store)
                body
            in
            let func =
              Func.State
                { name; cenv = cenv_local; lenv = lenv_local; body; transition }
            in
            (funcs @ [ func ], store))
          ([], store) states
      in
      (* Build a method apply *)
      (* Conceptually, parser is an object with a single method, apply *)
      let init = List.filter_map var_decl_to_stmt locals in
      let func_apply =
        Func.Parser
          { name = "apply"; params; cenv = cenv_local; lenv = LEnv.empty; init }
      in
      let obj =
        Object.Parser
          {
            tdenv = tdenv_local;
            tsto = tsto_local;
            vsto = vsto_local;
            funcs = func_apply :: funcs_state;
          }
      in
      let store = GSto.add path obj store in
      store
  (* Every time a control is instantiated, it causes:
     1 instantiation of each table defined within it
     every extern and control instantiation that is in the control source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclos.Control
      {
        tdenv = tdenv_local;
        cenv = cenv_local;
        tsto = tsto_local;
        vsto = vsto_local;
        params;
        tparams = _tparams;
        cparams;
        locals;
        apply;
        _;
      } ->
      let cenv_local = CEnv.enter cenv_local in
      let tsto_local = TSto.enter tsto_local in
      let vsto_local = VSto.enter vsto_local in
      let lenv_local = LEnv.empty in
      (* Add constructor arguments to constant environment and value store *)
      (* let tdenv_local = eval_targs cenv tdenv_local tparams typs in *)
      let names, typs, values, store =
        eval_args cenv tsto vsto tdenv ccenv store path cparams args
      in
      let cenv_local, tsto_local, vsto_local =
        fold_left3
          (fun (cenv, tsto, vsto) name typ value ->
            load_const cenv tsto vsto name typ value)
          (cenv_local, tsto_local, vsto_local)
          names typs values
      in
      (* Instantiate local instantiations, load constants and local variables *)
      let cenv_local, _lenv_local, tsto_local, vsto_local, store =
        List.fold_left
          (fun (cenv, lenv, tsto, vsto, store) local ->
            instantiate_control_local_decl cenv lenv tsto vsto tdenv_local ccenv
              store path local)
          (cenv_local, lenv_local, tsto_local, vsto_local, store)
          locals
      in
      (* Build a method apply *)
      (* Conceptually, control is an object with a single method, apply *)
      let init = List.filter_map var_decl_to_stmt locals in
      let cenv_local, tsto_local, vsto_local, store =
        List.fold_left
          (fun (cenv, tsto, vsto, store) stmt ->
            instantiate_stmt cenv tsto vsto tdenv ccenv store path stmt)
          (cenv_local, tsto_local, vsto_local, store)
          apply.statements
      in
      let body = init @ [ BlockStatement { tags = apply.tags; block = apply } ] in
      let func_apply =
        Func.Control
          {
            name = "apply";
            params;
            cenv = cenv_local;
            lenv = LEnv.empty;
            body;
          }
      in
      let obj =
        Object.Control
          {
            tdenv = tdenv_local;
            tsto = tsto_local;
            vsto = vsto_local;
            funcs = [ func_apply ];
          }
      in
      let store = GSto.add path obj store in
      store
  (* Others do not involve recursive instantiation other than the args *)
  | Cclos.Package { params; tparams = _tparams } ->
      let cenv_package = CEnv.enter cenv in
      let tsto_package = TSto.enter tsto in
      let vsto_package = VSto.enter vsto in
      (* Add constructor arguments to constant environment and value store *)
      (* let tdenv_package = eval_targs cenv tdenv_package tparams typs in *)
      let names, typs, values, store =
        eval_args cenv tsto vsto tdenv ccenv store path params args
      in
      let cenv_package, tsto_package, vsto_package =
        fold_left3
          (fun (cenv, tsto, vsto) name typ value ->
            load_const cenv tsto vsto name typ value)
          (cenv_package, tsto_package, vsto_package)
          names typs values
      in
      let obj =
        Object.Package
          {
            tdenv;
            cenv = cenv_package;
            tsto = tsto_package;
            vsto = vsto_package;
          }
      in
      let store = GSto.add path obj store in
      store
  | Cclos.Extern ->
      let obj = Object.Extern in
      let store = GSto.add path obj store in
      store

(* Instantiate from expression *)

and instantiate_expr (cenv : cenv) (tsto : tsto) (vsto : vsto) (tdenv : tdenv)
    (ccenv : ccenv) (store : store) (path : string list) (typ : Type.t)
    (args : Argument.t list) : store =
  let cclos, targs = cclos_from_type typ ccenv in
  let store =
    instantiate_cclos cenv tsto vsto tdenv ccenv store path cclos args targs
  in
  store

(* Instantiate from statement *)

and instantiate_stmt (cenv : cenv) (tsto : tsto) (vsto : vsto) (tdenv : tdenv)
    (ccenv : ccenv) (store : store) (path : string list) (stmt : Statement.t) :
    cenv * tsto * vsto * store =
  match stmt with
  (* (TODO) what happens if a direct application is inside a nested block? *)
  | BlockStatement { block; _ } ->
      let stmts = block.statements in
      List.fold_left
        (fun (cenv, tsto, vsto, store) stmt ->
          instantiate_stmt cenv tsto vsto tdenv ccenv store path stmt)
        (cenv, tsto, vsto, store) stmts
  | DirectApplication { typ; args; _ } ->
      let name = name_from_type typ in
      let cclos, targs = cclos_from_type typ ccenv in
      let store =
        instantiate_cclos cenv tsto vsto tdenv ccenv store (path @ [ name ])
          cclos args targs
      in
      let typ = Typ.Ref in
      let value = Value.Ref (path @ [ name ]) in
      let cenv, tsto, vsto = load_const cenv tsto vsto name typ value in
      (cenv, tsto, vsto, store)
  | _ -> (cenv, tsto, vsto, store)

(* Instantiate from declaration *)

and instantiate_parser_local_decl (cenv : cenv) (lenv : lenv) (tsto : tsto)
    (vsto : vsto) (tdenv : tdenv) (ccenv : ccenv) (store : store)
    (path : string list) (decl : Declaration.t) :
    cenv * lenv * tsto * vsto * store =
  (* parserLocalElement
     : constantDeclaration
     | instantiation
     | variableDeclaration
     | valueSetDeclaration; (13.2) *)
  match decl with
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let name = name.str in
      let obj = Object.ValueSet in
      let store = GSto.add (path @ [ name ]) obj store in
      let typ = Typ.Ref in
      let value = Value.Ref (path @ [ name ]) in
      let cenv, tsto, vsto = load_const cenv tsto vsto name typ value in
      (cenv, lenv, tsto, vsto, store)
  (* When using an expression for the size, the expression
     must be parenthesized and compile-time known. (7.1.6.2) *)
  | Variable { name; typ; _ } ->
      let name = name.str in
      let typ = Static.eval_typ tdenv cenv vsto typ in
      let lenv, tsto = load_var lenv tsto name typ in
      (cenv, lenv, tsto, vsto, store)
  | _ ->
      let cenv, tsto, vsto, _tdenv, _ccenv, store =
        instantiate_decl cenv tsto vsto tdenv ccenv store path decl
      in
      (cenv, lenv, tsto, vsto, store)

and instantiate_control_local_decl (cenv : cenv) (lenv : lenv) (tsto : tsto)
    (vsto : vsto) (tdenv : tdenv) (ccenv : ccenv) (store : store)
    (path : string list) (decl : Declaration.t) :
    cenv * lenv * tsto * vsto * store =
  (* controlLocalDeclaration
     : constantDeclaration
     | actionDeclaration
     | tableDeclaration
     | instantiation
     | variableDeclaration; (14) *)
  match decl with
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; properties; _ } ->
      let name = name.str in
      let obj = Object.Table { cenv; lenv; properties } in
      let store = GSto.add (path @ [ name ]) obj store in
      let typ = Typ.Ref in
      let value = Value.Ref (path @ [ name ]) in
      let cenv, tsto, vsto = load_const cenv tsto vsto name typ value in
      (cenv, lenv, tsto, vsto, store)
  (* When using an expression for the size, the expression
     must be parenthesized and compile-time known. (7.1.6.2) *)
  | Variable { name; typ; _ } ->
      let name = name.str in
      let typ = Static.eval_typ tdenv cenv vsto typ in
      let lenv, tsto = load_var lenv tsto name typ in
      (cenv, lenv, tsto, vsto, store)
  | _ ->
      let cenv, tsto, vsto, _tdenv, _ccenv, store =
        instantiate_decl cenv tsto vsto tdenv ccenv store path decl
      in
      (cenv, lenv, tsto, vsto, store)

and instantiate_decl (cenv : cenv) (tsto : tsto) (vsto : vsto) (tdenv : tdenv)
    (ccenv : ccenv) (store : store) (path : string list) (decl : Declaration.t)
    : cenv * tsto * vsto * tdenv * ccenv * store =
  match decl with
  (* Explicit instantiation *)
  | Instantiation { name; typ; args; _ } ->
      let name = name.str in
      let cclos, targs = cclos_from_type typ ccenv in
      let store =
        instantiate_cclos cenv tsto vsto tdenv ccenv store (path @ [ name ])
          cclos args targs
      in
      let typ = Typ.Ref in
      let value = Value.Ref (path @ [ name ]) in
      let cenv, tsto, vsto = load_const cenv tsto vsto name typ value in
      (cenv, tsto, vsto, tdenv, ccenv, store)
  (* Load declarations *)
  | _ ->
      let cenv, tsto, vsto, tdenv, ccenv =
        load_decl cenv tsto vsto tdenv ccenv decl
      in
      (cenv, tsto, vsto, tdenv, ccenv, store)

let instantiate_program (program : program) =
  let (Program decls) = program in
  let cenv = CEnv.empty in
  let tsto = TSto.empty in
  let vsto = VSto.empty in
  let tdenv = TDEnv.empty in
  let ccenv = CCEnv.empty in
  let store = GSto.empty in
  let _, _, _, _, _, store =
    List.fold_left
      (fun (cenv, tsto, vsto, tdenv, ccenv, store) decl ->
        instantiate_decl cenv tsto vsto tdenv ccenv store [] decl)
      (cenv, tsto, vsto, tdenv, ccenv, store)
      decls
  in
  store
