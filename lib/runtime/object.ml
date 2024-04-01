open Syntax
open Ast
open Utils
open Envs

(* Stateful objects *)
(* The stateful types of objects in P4_16 are
   packages, parsers, controls, externs, tables, and value-sets
   P4_16 functions are also considered to be in that group,
   even if they happen to be pure functions of their arguments (Appendix F) *)

type t =
  | Package of { env : env; tenv : tenv; tdenv : tdenv }
  | Parser of {
      env : env;
      tenv : tenv;
      tdenv : tdenv;
      lenv : lenv;
      params : Parameter.t list;
      locals : Declaration.t list;
      states : Parser.state list;
    }
  | Control of {
      env : env;
      tenv : tenv;
      tdenv : tdenv;
      lenv : lenv;
      params : Parameter.t list;
      locals : Declaration.t list;
      apply : Block.t;
    }
  | Extern
  | Table of {
      lenv : lenv;
      properties : Table.property list
    }
  | Function
  | ValueSet

(* Printer *)

let print ?(indent = 0) (obj : t) =
  match obj with
  | Package { env; tenv; _ } ->
      Printf.sprintf "%sPackage {\n%senv =\n%s\n%stenv =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (Env.print env ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (TEnv.print tenv ~indent:(indent + 3))
  | Parser { env; tenv; lenv; _ } ->
      Printf.sprintf "%sParser {\n%senv =\n%s\n%stenv =\n%s\n%slenv =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (Env.print env ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (TEnv.print tenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
  | Control { env; tenv; lenv; _ } ->
      Printf.sprintf "%sControl {\n%senv =\n%s\n%stenv =\n%s\n%slenv =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (Env.print env ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (TEnv.print tenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
  | Extern -> Printf.sprintf "%sExtern" (Print.print_indent indent)
  | Table { lenv; _ } ->
      Printf.sprintf "%sTable {\n%slenv =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
  | Function -> Printf.sprintf "%sFunction" (Print.print_indent indent)
  | ValueSet -> Printf.sprintf "%sValueSet" (Print.print_indent indent)
