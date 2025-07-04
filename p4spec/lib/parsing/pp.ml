open Il.Ast
open Xl
open Util.Source
open El.Ast
open Ast_utils
open Pp_utils
module Num = Num
module F = Format

let verbose = ref false

let pp_atom fmt (atom : atom) : unit =
  F.fprintf fmt "%s" (Atom.string_of_atom atom.it)

let pp_atoms fmt (atoms : atom list) : unit =
  match atoms with
  | [] -> F.fprintf fmt ""
  | _ ->
      let atoms =
        atoms
        |> List.map (fun atom -> F.asprintf "%a" pp_atom atom)
        |> List.map String.lowercase_ascii
      in
      F.fprintf fmt "%s" (String.concat "" atoms)

let rec pp_default_case_v fmt value : unit =
  match value.it with
  | CaseV (mixop, values) ->
      let len = List.length mixop + List.length values in
      List.init len (fun idx ->
          if idx mod 2 = 0 then
            idx / 2 |> List.nth mixop |> F.asprintf "%a" pp_atoms
          else idx / 2 |> List.nth values |> F.asprintf "%a" pp_value)
      |> List.filter (fun str -> str <> "")
      |> String.concat " "
      |>
      if !verbose then F.fprintf fmt "@%s_< %s>_@" (id_of_case_v value)
      else F.fprintf fmt "%s"
  | _ -> failwith "@pp_default_case_v: Expected CaseV value"

and pp_num fmt (num : num) : unit =
  match num with
  | `Nat n -> F.fprintf fmt "%s" (Bigint.to_string n)
  | `Int i ->
      F.fprintf fmt "%s"
        ((if i >= Bigint.zero then "" else "-")
        ^ Bigint.to_string (Bigint.abs i))

and pp_value fmt (value : value) : unit =
  match value.it with
  | BoolV b -> F.fprintf fmt "%b" b
  | NumV n -> F.fprintf fmt "%a" pp_num n
  | TextV _ -> pp_text_v fmt value
  | StructV _ -> failwith "not implemented"
  | CaseV _ -> pp_case_v fmt value
  | TupleV values ->
      F.fprintf fmt "(%s)"
        (String.concat ", "
           (List.map (fun v -> F.asprintf "%a" pp_value v) values))
  | OptV (Some v) -> F.fprintf fmt "%a" pp_value v
  | OptV None -> F.fprintf fmt ""
  | ListV [] -> F.fprintf fmt ""
  | ListV values ->
      F.fprintf fmt "%s"
        (String.concat ", "
           (List.map (fun v -> F.asprintf "%a" pp_value v) values))
  | _ -> failwith "@pp_value: TODO"

and pp_text_v fmt (value : value) : unit =
  match value.it with
  | TextV text -> F.fprintf fmt "%s" text
  | _ -> failwith "@pp_text_v: expected TextV value"

and pp_syntax_id fmt (value : value) : unit =
  match flatten_case_v value with
  | "identifier", [ [ "$" ]; [] ], [ value_text ] -> pp_text_v fmt value_text
  | "identifier", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_id: ill-formed identifier:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_id: expected identifier, got %s"
           (id_of_case_v value))

and pp_syntax_tid fmt (value : value) : unit =
  match flatten_case_v value with
  | "typeIdentifier", [ [ "@" ]; [] ], [ value_text ] ->
      pp_text_v fmt value_text
  | "typeIdentifier", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_tid: ill-formed typeIdentifier:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_tid: expected typeIdentifier, got %s"
           (id_of_case_v value))

and pp_syntax_name fmt (value : value) : unit =
  match flatten_case_v value with
  | "nonTypeName", [ [ "APPLY" ] ], [] -> F.fprintf fmt "apply"
  | "nonTypeName", [ [ "KEY" ] ], [] -> F.fprintf fmt "key"
  | "nonTypeName", [ [ "ACTIONS" ] ], [] -> F.fprintf fmt "actions"
  | "nonTypeName", [ [ "STATE" ] ], [] -> F.fprintf fmt "state"
  | "nonTypeName", [ [ "ENTRIES" ] ], [] -> F.fprintf fmt "entries"
  | "nonTypeName", [ [ "TYPE" ] ], [] -> F.fprintf fmt "type"
  | "nonTypeName", [ [ "PRIORITY" ] ], [] -> F.fprintf fmt "priority"
  | "prefixedNonTypeName", [ []; []; [] ], [ dot; non_type_name ] ->
      F.fprintf fmt "%a%a" pp_case_v dot pp_syntax_name non_type_name
  | "prefixedType", [ []; []; [] ], [ dot; tid ] ->
      F.fprintf fmt "%a%a" pp_case_v dot pp_syntax_tid tid
  | "name", [ [ "LIST" ] ], [] -> F.fprintf fmt "list"
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_name: expected name, got %s"
           (id_of_case_v value))

and pp_syntax_dir fmt (value : value) : unit =
  match flatten_case_v value with
  | "direction", [ [ "IN" ] ], [] -> F.fprintf fmt "in"
  | "direction", [ [ "OUT" ] ], [] -> F.fprintf fmt "out"
  | "direction", [ [ "INOUT" ] ], [] -> F.fprintf fmt "inout"
  | "direction", [ [ "NONE" ] ], [] -> F.fprintf fmt ""
  | "direction", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_dir: ill-formed direction:\n%a" pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_dir: expected direction, got %s"
           (id_of_case_v value))

and pp_syntax_type fmt (value : value) : unit =
  match flatten_case_v value with
  | "baseType", [ [ "BOOL" ] ], [] -> F.fprintf fmt "bool"
  | "baseType", [ [ "MATCH_KIND" ] ], [] -> F.fprintf fmt "match_kind"
  | "baseType", [ [ "ERROR" ] ], [] -> F.fprintf fmt "error"
  | "baseType", [ [ "BIT" ] ], [] -> F.fprintf fmt "bit"
  | "baseType", [ [ "STRING" ] ], [] -> F.fprintf fmt "string"
  | "baseType", [ [ "INT" ] ], [] -> F.fprintf fmt "int"
  | "baseType", [ [ "BIT"; "<" ]; [ ">" ] ], [ value_int ] ->
      F.fprintf fmt "bit<%a>" pp_value value_int
  | "baseType", [ [ "INT"; "<" ]; [ ">" ] ], [ value_int ] ->
      F.fprintf fmt "int<%a>" pp_value value_int
  | "baseType", [ [ "VARBIT"; "<" ]; [ ">" ] ], [ value_int ] ->
      F.fprintf fmt "varbit<%a>" pp_value value_int
  | "baseType", [ [ "BIT"; "<"; "(" ]; [ ")"; ">" ] ], [ expr ] ->
      F.fprintf fmt "bit<(%a)>" pp_syntax_expr expr
  | "baseType", [ [ "INT"; "<"; "(" ]; [ ")"; ">" ] ], [ expr ] ->
      F.fprintf fmt "int<(%a)>" pp_syntax_expr expr
  | "baseType", [ [ "VARBIT"; "<"; "(" ]; [ ")"; ">" ] ], [ expr ] ->
      F.fprintf fmt "varbit<(%a)>" pp_syntax_expr expr
  | "baseType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed base type:\n%a" pp_case_v value)
  | "specializedType", [ []; [ "<" ]; [ ">" ] ], [ type_name; type_arg_list ] ->
      F.fprintf fmt "%a<%a>" pp_syntax_name type_name pp_value type_arg_list
  | "specializedType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed specialized type:\n%a"
           pp_case_v value)
  | "headerStackType", [ []; [ "[" ]; [ "]" ] ], [ type_name; expr ] ->
      F.fprintf fmt "%a[%a]" pp_syntax_name type_name pp_syntax_expr expr
  | "headerStackType", [ []; [ "[" ]; [ "]"; "PHTM_16" ] ], [ spec_type; expr ]
    ->
      F.fprintf fmt "%a[%a]" pp_syntax_type spec_type pp_syntax_expr expr
  | "headerStackType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed header stack type:\n%a"
           pp_case_v value)
  | "listType", [ [ "LIST"; "<" ]; [ ">" ] ], [ type_arg ] ->
      F.fprintf fmt "list<%a>" pp_syntax_type_arg type_arg
  | "listType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed list type:\n%a" pp_case_v value)
  | "tupleType", [ [ "TUPLE"; "<" ]; [ ">" ] ], [ type_arg_list ] ->
      F.fprintf fmt "tuple<%a>" pp_value type_arg_list
  | "tupleType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed tuple type:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_type: expected type, got %s"
           (id_of_case_v value))

and pp_syntax_expr _fmt (value : value) : unit =
  match flatten_case_v value with
  | _ -> failwith "@pp_syntax_expr: not yet implemented"

and pp_syntax_type_arg _fmt (value : value) : unit =
  match flatten_case_v value with
  | _ -> failwith "@pp_syntax_type_arg: not yet implemented"

and pp_syntax_stmt _fmt (value : value) : unit =
  match flatten_case_v value with
  | _ -> failwith "@pp_syntax_stmt: not yet implemented"

and pp_syntax_mthd fmt (value : value) : unit =
  match flatten_case_v value with
  | "methodPrototype", [ []; []; [ ";" ] ], [ _anno; _func ]
  | "methodPrototype", [ []; [ "ABSTRACT" ]; [ ";" ] ], [ _anno; _func ]
  | "methodPrototype", [ []; []; [ "(" ]; [ ")"; ";" ] ], [ _anno; _; _func ] ->
      pp_default_case_v fmt value
  | "methodPrototype", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_method: ill-formed methodPrototype:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_method: expected methodPrototype, got %s"
           (id_of_case_v value))

and pp_syntax_block fmt (value : value) : unit =
  match flatten_case_v value with
  | "blockStatement", [ []; [ "{" ]; [ "}" ] ], [ _; _ ] ->
      F.fprintf fmt "%a" pp_default_case_v value
  | "blockStatement", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_block: ill-formed blockStatement:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_block: expected block, got %s"
           (id_of_case_v value))

and pp_syntax_decls ~level fmt (value : value) : unit =
  match value.it with
  | ListV values ->
      pp_list ~level (pp_syntax_decl ~level) ~sep:SemicolonNl fmt values
  | _ ->
      failwith
        (F.asprintf "@pp_syntax_decls: expected ListV, got %a" pp_value value)

and pp_syntax_decl ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "constantDeclaration",
      [ []; [ "CONST" ]; []; []; [ ";" ] ],
      [ _optAnnotations; _typeRef; _name; _init ] ) ->
      F.fprintf fmt ""
  | "errorDeclaration", _, _
  | "matchKindDeclaration", _, _
  | ( "externDeclaration",
      [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; _; _ ] )
  | "externDeclaration", _, _
  | "instantiation", _, _
  | "functionDeclaration", [ []; []; []; [] ], [ _; _; _ ]
  | "actionDeclaration", _, _
  | "parserDeclaration", _, _ ->
      pp_default_case_v fmt value
  | ( "controlDeclaration",
      [ []; []; [ "{" ]; [ "APPLY" ]; [ "}" ] ],
      [ control_type_decl; opt_constructor_params; control_local_decls; body ] )
    ->
      F.fprintf fmt "%a%a {\n%a\n%sapply %a\n%s}" pp_default_case_v
        control_type_decl pp_value opt_constructor_params
        (pp_syntax_decls ~level:(level + 1))
        control_local_decls
        (indent (level + 1))
        pp_syntax_block body (indent level)
  | ( "headerTypeDeclaration",
      [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; _; _ ] )
  | ( "headerUnionDeclaration",
      [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; _; _ ] )
  | ( "structTypeDeclaration",
      [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; _; _ ] )
  | "enumDeclaration", _, _
  | "typeDeclaration", [ []; [ ";" ] ], [ _ ]
  | "typeDeclaration", [ []; [ ";"; "PHTM_13" ] ], [ _ ]
  | "typeDeclaration", [ []; [ ";"; "PHTM_14" ] ], [ _ ]
  | "typeDeclaration", [ []; [ ";"; "PHTM_15" ] ], [ _ ]
  | "tableDeclaration", _, _ ->
      pp_default_case_v fmt value
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_decl: expected declaration, got %s"
           (id_of_case_v value))

and pp_case_v' fmt (value : value) : unit =
  match flatten_case_v value with
  (* Misc *)
  | "trailingComma", [ [ "," ]; [ "PHTM_0" ] ], [] -> F.fprintf fmt ","
  | "const", [ [ "CONST" ] ], [] -> F.fprintf fmt "const"
  (* Numbers *)
  | "number", [ []; [ "PHTM_1" ] ], [ value_int ] ->
      F.fprintf fmt "%a" pp_value value_int
  | "number", [ []; [ "S" ]; [] ], [ value_width; value_int ] ->
      F.fprintf fmt "%as%a" pp_value value_width pp_value value_int
  | "number", [ []; [ "W" ]; [] ], [ value_width; value_int ] ->
      F.fprintf fmt "%aw%a" pp_value value_width pp_value value_int
  (* Strings *)
  | "stringLiteral", [ []; [ "PHTM_2" ] ], [ value_text ] ->
      pp_value fmt value_text
  (* Names *)
  | "dotPrefix", [ [ "." ] ], [] -> F.fprintf fmt "."
  | "direction", [ [ "NONE" ] ], [] -> F.fprintf fmt ""
  | _ -> pp_default_case_v fmt value

and pp_case_v fmt (value : value) : unit =
  match id_of_case_v value with
  | "constantDeclaration" | "errorDeclaration" | "matchKindDeclaration"
  | "externDeclaration" | "instantiation" | "functionDeclaration"
  | "actionDeclaration" | "parserDeclaration" | "controlDeclaration"
  | "headerTypeDeclaration" | "headerUnionDeclaration" | "structTypeDeclaration"
  | "enumDeclaration" | "typeDeclaration" ->
      pp_syntax_decl ~level:0 fmt value
  | "nonTypeName" | "name" | "prefixedNonTypeName" -> pp_syntax_name fmt value
  | "typeIdentifier" -> pp_syntax_tid fmt value
  | "identifier" -> pp_syntax_id fmt value
  | "direction" -> pp_syntax_dir fmt value
  | _ -> pp_case_v' fmt value
