open Syntax
open Ast

(* Environments *)

type env = Env.t
type tenv = Tenv.t

(* Compile-time evaluation of type simplification *)

let rec eval_simplify_typ (tenv : tenv) (typ : Typ.t) : Typ.t =
  match typ with
  | Typ.Name { name } -> eval_simplify_typ tenv (Tenv.find name tenv)
  | Typ.NewType { name } -> Tenv.find name tenv
  | _ -> typ

let rec eval_typ (env : env) (tenv : tenv) (typ : Type.t) : Typ.t =
  match typ with
  | Bool _ -> Typ.Bool
  | Integer _ -> Typ.AInt
  | IntType { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      Typ.Bit { width }
  | BitType { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      Typ.Bit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      Typ.Bit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ env tenv header in
      let size = eval_expr env tenv size |> Value.extract_bigint in
      Typ.Array { typ = header; size }
  | String _ -> Typ.String
  | Error _ -> Typ.Error
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ env tenv) args in
      Typ.Tuple vargs
  | TypeName { name = BareName text; _ } ->
      let var = text.str in
      Tenv.find var tenv
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Tenv.find_toplevel var tenv
  | _ ->
      Printf.sprintf "(TODO: eval_typ) %s" (Pretty.print_type typ) |> failwith

(* Compile-time evaluation of expressions *)

and eval_expr (env : env) (tenv : tenv) (expr : Expression.t) : Value.t =
  match expr with
  | True _ -> Value.Bool true
  | False _ -> Value.Bool false
  | Int { i; _ } -> (
      let value = i.value in
      match i.width_signed with
      | Some (width, signed) ->
          if signed then Value.Int { value; width }
          else Value.Bit { value; width }
      | None -> Value.AInt value)
  | String { text; _ } -> Value.String text.str
  | Name { name = BareName text; _ } ->
      let var = text.str in
      Env.find var env
  | Name { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Env.find_toplevel var env
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr env tenv bits in
      let vlo = eval_expr env tenv lo in
      let vhi = eval_expr env tenv hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr env tenv) values in
      Value.Tuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr env tenv entry.value in
            (key, value))
          entries
      in
      Value.Struct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr env tenv arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr env tenv arg_fst in
      let varg_snd = eval_expr env tenv arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ env tenv typ in
      let typ = eval_simplify_typ tenv typ in
      let vexpr = eval_expr env tenv expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr env tenv expr in
      let name = name.str in
      match vexpr with
      | Value.Header { entries; _ } | Value.Struct { entries } ->
          List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" (Value.print vexpr)
          |> failwith)
  | _ ->
      Printf.sprintf "(TODO: eval_expr) %s" (Pretty.print_expr expr) |> failwith
