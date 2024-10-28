module Pp = C.Pp
module C = C.Syntax
module Il = Il.Ast
module S = Util.Source
module L = Lang.Ast
module T = Runtime.Types
module V = Runtime.Value

exception CompileError of string
exception V1Model of string

let counter = ref 0

let get_var_name (var : Il.var) : string =
  match S.it var with Top x | Current x -> S.it x

let type_name (typ : Il.typ) : string =
  match S.it typ with
  | T.StructT (name, _) -> name
  | _ -> failwith "Not implemented"

let fetch_inst_args (inst_decl : Il.decl) : Il.arg list =
  match S.it inst_decl with
  | Il.InstD { args; _ } -> args
  | _ -> raise (CompileError "Expected an instantiation declaration")

let fetch_targs (inst_decl : Il.decl) : Il.typ list =
  match S.it inst_decl with
  | Il.InstD { targs; _ } -> targs
  | _ -> raise (CompileError "Expected an instantiation declaration")

(* fetch_parser_inst will fetch the name of the parser component
   instantiated in the main instantiation declaration. This name
   can be used to fetch the actual parser declarations *)
let fetch_parser_inst_name (inst_decl : Il.decl) : string =
  let args = fetch_inst_args inst_decl in
  let parser_arg_opt =
    List.find_opt
      (fun arg ->
        match S.it arg with
        | L.ExprA x -> (
            match (S.note x : Il.note) with
            | { typ = ParserT _; _ } -> true
            | _ -> false)
        | _ -> false)
      args
  in
  let parser_arg =
    match parser_arg_opt with
    | None ->
        raise (V1Model "Expected a parser component in V1Switch instantiation")
    | Some x -> x
  in
  match S.it parser_arg with
  | L.ExprA { it; _ } -> (
      match it with
      | Il.InstE { var_inst; _ } -> get_var_name var_inst
      | _ -> raise (CompileError "Expected an instantiation expression"))
  | _ -> raise (CompileError "Expected an expression argument")

(* fetch_control_insts will fetch the names of the control components
   instantiated in the main instantiation declaration. These names
   can be used to fetch the actual control declarations *)
let fetch_control_inst_names (inst_decl : Il.decl) : string list =
  let args = fetch_inst_args inst_decl in
  let filtered_args =
    List.filter
      (fun arg ->
        match S.it arg with
        | L.ExprA x -> (
            match (S.note x : Il.note) with
            | { typ = ControlT _; _ } -> true
            | _ -> false)
        | _ -> false)
      args
  in
  let () =
    if List.length filtered_args <> 5 then
      raise (V1Model "Expected 5 control components in V1Switch instantiation")
  in
  List.map
    (fun arg ->
      match S.it arg with
      | L.ExprA { it; _ } -> (
          match it with
          | Il.InstE { var_inst; _ } -> get_var_name var_inst
          | _ -> raise (CompileError "Expected an instantiation expression"))
      | _ -> raise (CompileError "Expected an expression argument"))
    filtered_args

let fetch_parser (inst_decl : Il.decl) (program : Il.program) : Il.decl =
  let parser_name = fetch_parser_inst_name inst_decl in
  List.find
    (fun (decl : Il.decl' S.phrase) ->
      match S.it decl with
      | Il.ParserD { id; _ } -> S.it id = parser_name
      | _ -> false)
    program

let fetch_control (inst_decl : Il.decl) (program : Il.program) : Il.decl list =
  let control_names = fetch_control_inst_names inst_decl in
  List.map
    (fun control_name ->
      List.find
        (fun (decl : Il.decl' S.phrase) ->
          match S.it decl with
          | Il.ControlD { id; _ } -> S.it id = control_name
          | _ -> false)
        program)
    control_names

let fetch_instantiate (program : Il.program) (arch : string) : Il.decl =
  List.find
    (fun (decl : Il.decl' S.phrase) ->
      match S.it decl with
      | Il.InstD { var_inst; _ } -> get_var_name var_inst = arch
      | _ -> false)
    program

let create_temporary () : C.cvar =
  let name = Printf.sprintf "__tmp%d" !counter in
  counter := !counter + 1;
  name

let is_lval (expr : Il.expr) : bool =
  match S.it expr with Il.VarE _ -> true | Il.ExprAccE _ -> true | _ -> false

let get_type_from_note (note : Il.note) : T.typ =
  match note with { typ; _ } -> typ

let is_extern_type (typ : T.typ) : bool =
  match typ with T.ExternT _ -> true | _ -> false

let get_extern_type_name (typ : T.typ) : string =
  match typ with
  | T.ExternT (id, _) -> id
  | _ -> failwith "Expected an extern type"
