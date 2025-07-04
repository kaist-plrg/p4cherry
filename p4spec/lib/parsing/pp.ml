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
  | OptV _ -> pp_opt_v fmt value
  | ListV _ -> pp_list_v fmt value
  | _ -> failwith "@pp_value: TODO"

and pp_text_v fmt (value : value) : unit =
  match value.it with
  | TextV text -> F.fprintf fmt "%s" text
  | _ -> failwith "@pp_text_v: expected TextV value"

and pp_opt_v ?(postfix = "") fmt (value : value) : unit =
  match value.it with
  | OptV (Some v) -> F.fprintf fmt "%a%s" pp_value v postfix
  | OptV None -> F.fprintf fmt ""
  | _ -> failwith "@pp_opt_v: expected OptV value"

and pp_list_v ?(sep = Comma) fmt (value : value) : unit =
  match (id_of_list_v value, value.it) with
  | "declaration", ListV [] -> F.fprintf fmt ";"
  | "declaration", ListV _ -> pp_syntax_decls ~level:0 fmt value
  | _, ListV [] -> ()
  | _, ListV values ->
      values
      |> List.map (F.asprintf "%a" pp_value)
      |> String.concat (F.asprintf "%a" pp_sep sep)
      |> F.fprintf fmt "%s"
  | _ -> failwith "@pp_list_v: expected ListV value"

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
      F.fprintf fmt "%a%a" pp_case_v dot pp_case_v non_type_name
  | "prefixedType", [ []; []; [] ], [ dot; tid ] ->
      F.fprintf fmt "%a%a" pp_case_v dot pp_case_v tid
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
      F.fprintf fmt "bit<(%a)>" pp_value expr
  | "baseType", [ [ "INT"; "<"; "(" ]; [ ")"; ">" ] ], [ expr ] ->
      F.fprintf fmt "int<(%a)>" pp_value expr
  | "baseType", [ [ "VARBIT"; "<"; "(" ]; [ ")"; ">" ] ], [ expr ] ->
      F.fprintf fmt "varbit<(%a)>" pp_value expr
  | "baseType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed base type:\n%a" pp_case_v value)
  | "specializedType", [ []; [ "<" ]; [ ">" ] ], [ type_name; type_arg_list ] ->
      F.fprintf fmt "%a<%a>" pp_case_v type_name (pp_list_v ~sep:Comma)
        type_arg_list
  | "specializedType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed specialized type:\n%a"
           pp_case_v value)
  | "headerStackType", [ []; [ "[" ]; [ "]" ] ], [ type_name; expr ] ->
      F.fprintf fmt "%a[%a]" pp_case_v type_name pp_value expr
  | "headerStackType", [ []; [ "[" ]; [ "]"; "PHTM_16" ] ], [ spec_type; expr ]
    ->
      F.fprintf fmt "%a[%a]" pp_case_v spec_type pp_value expr
  | "headerStackType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed header stack type:\n%a"
           pp_case_v value)
  | "listType", [ [ "LIST"; "<" ]; [ ">" ] ], [ type_arg ] ->
      F.fprintf fmt "list<%a>" pp_case_v type_arg
  | "listType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed list type:\n%a" pp_case_v value)
  | "tupleType", [ [ "TUPLE"; "<" ]; [ ">" ] ], [ type_arg_list ] ->
      F.fprintf fmt "tuple<%a>" (pp_list_v ~sep:Comma) type_arg_list
  | "tupleType", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_type: ill-formed tuple type:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_type: expected type, got %s"
           (id_of_case_v value))

and pp_syntax_tparams fmt (value : value) : unit =
  match flatten_case_v value with
  | "typeParameters", [ [ "<" ]; [ ">" ] ], [ tparams ] ->
      F.fprintf fmt "<%a>" (pp_list_v ~sep:Comma) tparams
  | "typeParameters", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_tparams: ill-formed type parameters:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_tparams: expected type parameters, got %s"
           (id_of_case_v value))

and pp_syntax_params fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "parameter",
      [ []; []; []; []; [] ],
      [ opt_annotations; dir; type_ref; name ] ) ->
      F.fprintf fmt "%a%a %a %a" (pp_opt_v ~postfix:" ") opt_annotations
        pp_case_v dir pp_case_v type_ref pp_case_v name
  | ( "parameter",
      [ []; []; []; []; []; [] ],
      [ opt_annotations; dir; type_ref; name; init ] ) ->
      F.fprintf fmt "%a%a %a %a%a" (pp_opt_v ~postfix:" ") opt_annotations
        pp_case_v dir pp_case_v type_ref pp_case_v name pp_case_v init
  | "parameter", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_params: ill-formed parameter:\n%a" pp_case_v
           value)
  | "constructorParameters", [ [ "(" ]; [ ")" ] ], [ params ] ->
      F.fprintf fmt "(%a)" (pp_list_v ~sep:Comma) params
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_params: expected parameter, got %s"
           (id_of_case_v value))

and pp_syntax_nb_expr fmt (value : value) : unit =
  let is_binary_op = function
    | "*" | "/" | "%" | "+" | "|+|" | "-" | "|-|" | "<<" | ">>" | "<=" | ">="
    | "<" | ">" | "!=" | "==" | "&" | "^" | "|" | "++" | "&&" | "||" ->
        true
    | _ -> false
  in
  match flatten_case_v value with
  | "nonBraceExpression", [ [ "TRUE" ] ], [] -> F.fprintf fmt "true"
  | "nonBraceExpression", [ [ "FALSE" ] ], [] -> F.fprintf fmt "false"
  | "nonBraceExpression", [ [ "THIS" ] ], [] -> F.fprintf fmt "this"
  | "nonBraceExpression", [ []; [ "[" ]; [ "]" ] ], [ array; index ] ->
      F.fprintf fmt "(%a)[%a]" pp_case_v array pp_case_v index
  | "nonBraceExpression", [ []; [ "[" ]; [ ":" ]; [ "]" ] ], [ bits; hi; lo ] ->
      F.fprintf fmt "(%a)[%a:%a]" pp_case_v bits pp_case_v hi pp_case_v lo
  | "nonBraceExpression", [ [ "!" ]; [] ], [ arg ] ->
      F.fprintf fmt "!(%a)" pp_case_v arg
  | "nonBraceExpression", [ [ "~" ]; [] ], [ arg ] ->
      F.fprintf fmt "~(%a)" pp_case_v arg
  | "nonBraceExpression", [ [ "-" ]; [] ], [ arg ] ->
      F.fprintf fmt "-(%a)" pp_case_v arg
  | "nonBraceExpression", [ [ "+" ]; [] ], [ arg ] ->
      F.fprintf fmt "+(%a)" pp_case_v arg
  | "nonBraceExpression", [ []; [ "." ]; [] ], [ typ; member ] ->
      F.fprintf fmt "(%a).(%a)" pp_case_v typ pp_case_v member
  | "nonBraceExpression", [ [ "ERROR"; "." ]; [] ], [ member ] ->
      F.fprintf fmt "error.(%a)" pp_case_v member
  | "nonBraceExpression", [ []; [ "." ]; [ "PHTM_5" ] ], [ expr; member ] ->
      F.fprintf fmt "(%a).(%a)" pp_case_v expr pp_case_v member
  | "nonBraceExpression", [ []; [ binop ]; [] ], [ arg1; arg2 ]
    when is_binary_op binop ->
      F.fprintf fmt "(%a) %s (%a)" pp_case_v arg1 binop pp_case_v arg2
  | ( "nonBraceExpression",
      [ []; [ "?" ]; [ ":" ]; [] ],
      [ cond; true_expr; false_expr ] ) ->
      F.fprintf fmt "(%a) ? (%a) : (%a)" pp_case_v cond pp_case_v true_expr
        pp_case_v false_expr
  | ( "nonBraceExpression",
      [ []; [ "<" ]; [ ">"; "(" ]; [ ")" ] ],
      [ func; type_args; args ] ) ->
      F.fprintf fmt "(%a)<%a>(%a)" pp_case_v func (pp_list_v ~sep:Comma)
        type_args (pp_list_v ~sep:Comma) args
  | "nonBraceExpression", [ []; [ "(" ]; [ ")" ] ], [ func; args ] ->
      F.fprintf fmt "(%a)(%a)" pp_case_v func (pp_list_v ~sep:Comma) args
  | "nonBraceExpression", [ []; [ "(" ]; [ ")"; "PHTM_6" ] ], [ typ; args ] ->
      F.fprintf fmt "(%a)(%a)" pp_case_v typ (pp_list_v ~sep:Comma) args
  | "nonBraceExpression", [ [ "(" ]; [ ")" ]; [] ], [ typ; expr ] ->
      F.fprintf fmt "(%a)(%a)" pp_case_v typ pp_case_v expr
  | "nonBraceExpression", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_nb_expr: ill-formed non-brace expression:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_nb_expr: expected non-brace expression, got %s"
           (id_of_case_v value))

and pp_syntax_expr fmt (value : value) : unit =
  let is_binary_op = function
    | "*" | "/" | "%" | "+" | "|+|" | "-" | "|-|" | "<<" | ">>" | "<=" | ">="
    | "<" | ">" | "!=" | "==" | "&" | "^" | "|" | "++" | "&&" | "||" ->
        true
    | _ -> false
  in
  match flatten_case_v value with
  | "expression", [ [ "..." ] ], [] -> F.fprintf fmt "..."
  | "expression", [ [ "TRUE" ] ], [] -> F.fprintf fmt "true"
  | "expression", [ [ "FALSE" ] ], [] -> F.fprintf fmt "false"
  | "expression", [ [ "THIS" ] ], [] -> F.fprintf fmt "this"
  | "expression", [ []; []; [] ], [ dot; name ] ->
      F.fprintf fmt "%a%a" pp_case_v dot pp_case_v name
  | "expression", [ []; [ "[" ]; [ "]" ] ], [ array; index ] ->
      F.fprintf fmt "(%a)[%a]" pp_case_v array pp_case_v index
  | "expression", [ []; [ "[" ]; [ ":" ]; [ "]" ] ], [ bits; hi; lo ] ->
      F.fprintf fmt "(%a)[%a:%a]" pp_case_v bits pp_case_v hi pp_case_v lo
  | "expression", [ [ "{" ]; []; [ "}" ] ], [ exprs; comma ] ->
      F.fprintf fmt "{ %a%a }" (pp_list_v ~sep:Comma) exprs
        (pp_opt_v ~postfix:"") comma
  | "expression", [ [ "INVALID" ] ], [] -> F.fprintf fmt "{#}"
  | "expression", [ [ "{" ]; []; [ "}"; "PHTM_7" ] ], [ kvs; comma ] ->
      F.fprintf fmt "{ %a%a }" (pp_list_v ~sep:Comma) kvs (pp_opt_v ~postfix:"")
        comma
  | "expression", [ [ "{" ]; [ ","; "..." ]; [ "}" ] ], [ kvs; comma ] ->
      F.fprintf fmt "{ %a, ...%a }" (pp_list_v ~sep:Comma) kvs
        (pp_opt_v ~postfix:"") comma
  | "expression", [ [ "!" ]; [] ], [ arg ] ->
      F.fprintf fmt "!(%a)" pp_case_v arg
  | "expression", [ [ "~" ]; [] ], [ arg ] ->
      F.fprintf fmt "~(%a)" pp_case_v arg
  | "expression", [ [ "-" ]; [] ], [ arg ] ->
      F.fprintf fmt "-(%a)" pp_case_v arg
  | "expression", [ [ "+" ]; [] ], [ arg ] ->
      F.fprintf fmt "+(%a)" pp_case_v arg
  | "expression", [ [ "(" ]; [ ")" ]; [] ], [ typ; expr ] ->
      F.fprintf fmt "(%a)(%a)" pp_case_v typ pp_case_v expr
  | "expression", [ []; [ "." ]; [] ], [ typ; name ] ->
      F.fprintf fmt "(%a).(%a)" pp_case_v typ pp_case_v name
  | "expression", [ [ "ERROR"; "." ]; [] ], [ member ] ->
      F.fprintf fmt "error.(%a)" pp_case_v member
  | "expression", [ []; [ "." ]; [ "PHTM_5" ] ], [ expr; member ] ->
      F.fprintf fmt "(%a).(%a)" pp_case_v expr pp_case_v member
  | "expression", [ []; [ binop ]; [] ], [ arg1; arg2 ] when is_binary_op binop
    ->
      F.fprintf fmt "(%a) %s (%a)" pp_case_v arg1 binop pp_case_v arg2
  | "expression", [ []; [ "?" ]; [ ":" ]; [] ], [ cond; true_expr; false_expr ]
    ->
      F.fprintf fmt "(%a) ? (%a) : (%a)" pp_case_v cond pp_case_v true_expr
        pp_case_v false_expr
  | ( "expression",
      [ []; [ "<" ]; [ ">"; "(" ]; [ ")" ] ],
      [ func; type_args; args ] ) ->
      F.fprintf fmt "(%a)<%a>(%a)" pp_case_v func (pp_list_v ~sep:Comma)
        type_args (pp_list_v ~sep:Comma) args
  | "expression", [ []; [ "(" ]; [ ")" ] ], [ func; args ] ->
      F.fprintf fmt "(%a)(%a)" pp_case_v func (pp_list_v ~sep:Comma) args
  | "expression", [ []; [ "(" ]; [ ")"; "PHTM_6" ] ], [ typ; args ] ->
      F.fprintf fmt "(%a)(%a)" pp_case_v typ (pp_list_v ~sep:Comma) args
  | "expression", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_expr: ill-formed expression:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_nb_expr: expected expression, got %s"
           (id_of_case_v value))

and pp_syntax_type_arg _fmt (value : value) : unit =
  match flatten_case_v value with
  | _ -> failwith "@pp_syntax_type_arg: not yet implemented"

and pp_syntax_init fmt (value : value) : unit =
  match flatten_case_v value with
  | "initializer", [ [ "=" ]; [] ], [ expr ] ->
      F.fprintf fmt " = %a" pp_syntax_expr expr
  | "initializer", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_init: ill-formed initializer:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_init: expected initializer, got %s"
           (id_of_case_v value))

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
  | ListV values -> pp_list ~level (pp_syntax_decl ~level) ~sep:Nl fmt values
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
  (* Type references *)
  | "typeOrVoid", [ [ "VOID" ] ], [] -> F.fprintf fmt "void"
  | _ -> pp_default_case_v fmt value

and pp_case_v fmt (value : value) : unit =
  match id_of_case_v value with
  | "constantDeclaration" | "errorDeclaration" | "matchKindDeclaration"
  | "externDeclaration" | "instantiation" | "functionDeclaration"
  | "actionDeclaration" | "parserDeclaration" | "controlDeclaration"
  | "headerTypeDeclaration" | "headerUnionDeclaration" | "structTypeDeclaration"
  | "enumDeclaration" | "typeDeclaration" ->
      pp_syntax_decl ~level:0 fmt value
  | "nonTypeName" | "name" | "prefixedNonTypeName" | "prefixedType" ->
      pp_syntax_name fmt value
  | "typeIdentifier" -> pp_syntax_tid fmt value
  | "identifier" -> pp_syntax_id fmt value
  | "direction" -> pp_syntax_dir fmt value
  | "baseType" | "specializedType" | "headerStackType" | "listType"
  | "tupleType" ->
      pp_syntax_type fmt value
  | "typeParameters" -> pp_syntax_tparams fmt value
  | "parameter" | "constructorParameters" -> pp_syntax_params fmt value
  | "nonBraceExpression" -> pp_syntax_nb_expr fmt value
  | "expression" -> pp_syntax_expr fmt value
  | _ -> pp_case_v' fmt value
