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
  | OptV _ -> pp_opt_v pp_value fmt value
  | ListV _ -> pp_list_v ~sep:Nl fmt value
  | _ -> failwith "@pp_value: TODO"

and pp_text_v fmt (value : value) : unit =
  match value.it with
  | TextV text -> F.fprintf fmt "%s" text
  | _ -> failwith "@pp_text_v: expected TextV value"

and pp_opt_v ?(postfix = "") pp_v fmt (value : value) : unit =
  match value.it with
  | OptV (Some v) -> F.fprintf fmt "%a%s" pp_v v postfix
  | OptV None -> F.fprintf fmt ""
  | _ -> failwith "@pp_opt_v: expected OptV value"

and pp_list_v ?(level = 0) ~sep fmt (value : value) : unit =
  let values =
    match value.it with
    | ListV values -> values
    | _ ->
        failwith
          (F.asprintf "@pp_list_v: expected ListV, got %a" pp_value value)
  in
  match id_of_list_v value with
  | "identifier" | "typeParameterList" | "parameter" | "expression" | "kvPair"
  | "simpleKeysetExpression" | "realTypeArg" | "typeArg" | "argument"
  | "keyElement" | "entry" | "selectCase" | "specifiedIdentifier"
  | "structField" | "simpleAnnotation" ->
      pp_list ~level pp_case_v ~sep fmt values
  | "switchCase" -> pp_list ~level (pp_syntax_stmt' ~level) ~sep fmt values
  | "declOrAssignmentOrMethodCallStatement" ->
      pp_list pp_syntax_decl_or_assign_or_call_stmt ~sep fmt values
  | "assignmentOrMethodCallStatement" ->
      pp_list (pp_syntax_stmt' ~level:0) ~sep fmt values
  | "statementOrDeclaration" ->
      pp_list ~level (pp_syntax_stat_or_decl ~level) ~sep fmt values
  | "methodPrototype" -> pp_list ~level (pp_syntax_mthd ~level) ~sep fmt values
  | "objDeclaration" | "controlLocalDeclaration" | "parserLocalElement" ->
      pp_list ~level (pp_syntax_decl ~level) ~sep fmt values
  | "action" -> pp_list ~level (pp_syntax_action ~level) ~sep fmt values
  | "tableProperty" ->
      pp_list ~level (pp_syntax_table_prop ~level) ~sep fmt values
  | "parserStatement" ->
      pp_list ~level (pp_syntax_parser_stmt ~level) ~sep fmt values
  | "parserState" ->
      pp_list ~level (pp_syntax_parser_state ~level) ~sep fmt values
  | "annotation" -> pp_list_no_start_indent ~level pp_case_v ~sep fmt values
  | "declaration" when List.compare_length_with values 0 = 0 ->
      F.fprintf fmt ";"
  | "declaration" -> pp_list ~level (pp_syntax_decl ~level) ~sep fmt values
  | _ ->
      failwith
        (Printf.sprintf "@pp_list_v: unknown ListV: %s" (id_of_list_v value))

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
  | "direction", [ [ "IN" ] ], [] -> F.fprintf fmt "in "
  | "direction", [ [ "OUT" ] ], [] -> F.fprintf fmt "out "
  | "direction", [ [ "INOUT" ] ], [] -> F.fprintf fmt "inout "
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
      F.fprintf fmt "%a<%a>" pp_case_v type_name
        (pp_list_v ~level:0 ~sep:Comma)
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
      F.fprintf fmt "tuple<%a>" (pp_list_v ~level:0 ~sep:Comma) type_arg_list
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
      F.fprintf fmt "<%a>" (pp_list_v ~level:0 ~sep:Comma) tparams
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
  | "parameter", [ []; []; []; []; [] ], [ opt_annos; dir; type_ref; name ] ->
      F.fprintf fmt "%a%a%a %a"
        (pp_opt_annos ~level:0 ~sep:SpaceSep)
        opt_annos pp_case_v dir pp_case_v type_ref pp_case_v name
  | ( "parameter",
      [ []; []; []; []; []; [] ],
      [ opt_annos; dir; type_ref; name; init ] ) ->
      F.fprintf fmt "%a%a%a %a%a"
        (pp_opt_annos ~level:0 ~sep:SpaceSep)
        opt_annos pp_case_v dir pp_case_v type_ref pp_case_v name pp_case_v init
  | "parameter", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_params: ill-formed parameter:\n%a" pp_case_v
           value)
  | "constructorParameters", [ [ "(" ]; [ ")" ] ], [ params ] ->
      F.fprintf fmt "(%a)" (pp_list_v ~level:0 ~sep:Comma) params
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
      F.fprintf fmt "%a.%a" pp_case_v typ pp_case_v member
  | "nonBraceExpression", [ [ "ERROR"; "." ]; [] ], [ member ] ->
      F.fprintf fmt "error.%a" pp_case_v member
  | "nonBraceExpression", [ []; [ "." ]; [ "PHTM_5" ] ], [ expr; member ] ->
      F.fprintf fmt "%a.%a" pp_case_v expr pp_case_v member
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
      F.fprintf fmt "%a<%a>(%a)" pp_case_v func
        (pp_list_v ~level:0 ~sep:Comma)
        type_args
        (pp_list_v ~level:0 ~sep:Comma)
        args
  | "nonBraceExpression", [ []; [ "(" ]; [ ")" ] ], [ func; args ] ->
      F.fprintf fmt "%a(%a)" pp_case_v func (pp_list_v ~level:0 ~sep:Comma) args
  | "nonBraceExpression", [ []; [ "(" ]; [ ")"; "PHTM_6" ] ], [ typ; args ] ->
      F.fprintf fmt "%a(%a)" pp_case_v typ (pp_list_v ~level:0 ~sep:Comma) args
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
      F.fprintf fmt "%a[%a]" pp_case_v array pp_case_v index
  | "expression", [ []; [ "[" ]; [ ":" ]; [ "]" ] ], [ bits; hi; lo ] ->
      F.fprintf fmt "%a[%a:%a]" pp_case_v bits pp_case_v hi pp_case_v lo
  | "expression", [ [ "{" ]; []; [ "}" ] ], [ exprs; comma ] ->
      F.fprintf fmt "{ %a%a }"
        (pp_list_v ~level:0 ~sep:Comma)
        exprs
        (pp_opt_v ~postfix:"" pp_case_v)
        comma
  | "expression", [ [ "INVALID" ] ], [] -> F.fprintf fmt "{#}"
  | "expression", [ [ "{" ]; []; [ "}"; "PHTM_7" ] ], [ kvs; comma ] ->
      F.fprintf fmt "{ %a%a }"
        (pp_list_v ~level:0 ~sep:Comma)
        kvs
        (pp_opt_v ~postfix:"" pp_case_v)
        comma
  | "expression", [ [ "{" ]; [ ","; "..." ]; [ "}" ] ], [ kvs; comma ] ->
      F.fprintf fmt "{ %a, ...%a }"
        (pp_list_v ~level:0 ~sep:Comma)
        kvs
        (pp_opt_v ~postfix:"" pp_case_v)
        comma
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
      F.fprintf fmt "%a.%a" pp_case_v typ pp_case_v name
  | "expression", [ [ "ERROR"; "." ]; [] ], [ member ] ->
      F.fprintf fmt "error.%a" pp_case_v member
  | "expression", [ []; [ "." ]; [ "PHTM_5" ] ], [ expr; member ] ->
      F.fprintf fmt "%a.%a" pp_case_v expr pp_case_v member
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
      F.fprintf fmt "%a<%a>(%a)" pp_case_v func
        (pp_list_v ~level:0 ~sep:Comma)
        type_args
        (pp_list_v ~level:0 ~sep:Comma)
        args
  | "expression", [ []; [ "(" ]; [ ")" ] ], [ func; args ] ->
      F.fprintf fmt "%a(%a)" pp_case_v func (pp_list_v ~level:0 ~sep:Comma) args
  | "expression", [ []; [ "(" ]; [ ")"; "PHTM_6" ] ], [ typ; args ] ->
      F.fprintf fmt "%a(%a)" pp_case_v typ (pp_list_v ~level:0 ~sep:Comma) args
  | "expression", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_expr: ill-formed expression:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_expr: expected expression, got %s"
           (id_of_case_v value))

and pp_syntax_keyset_expr fmt (value : value) : unit =
  match flatten_case_v value with
  | "simpleKeysetExpression", [ []; [ "&&&" ]; [] ], [ expr; mask ] ->
      F.fprintf fmt "(%a) &&& (%a)" pp_case_v expr pp_case_v mask
  | "simpleKeysetExpression", [ []; [ ".." ]; [] ], [ lo; hi ] ->
      F.fprintf fmt "(%a) .. (%a)" pp_case_v lo pp_case_v hi
  | "simpleKeysetExpression", [ [ "_" ] ], [] -> F.fprintf fmt "_"
  | "simpleKeysetExpression", [ [ "DEFAULT" ] ], [] -> F.fprintf fmt "default"
  | "simpleKeysetExpression", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_keyset_expr: ill-formed simple keyset expression:\n%a"
           pp_case_v value)
  | "reducedSimpleKeysetExpression", [ []; [ "&&&" ]; [] ], [ expr; mask ] ->
      F.fprintf fmt "(%a) &&& (%a)" pp_case_v expr pp_case_v mask
  | "reducedSimpleKeysetExpression", [ []; [ ".." ]; [] ], [ lo; hi ] ->
      F.fprintf fmt "(%a) .. (%a)" pp_case_v lo pp_case_v hi
  | "reducedSimpleKeysetExpression", [ [ "_" ] ], [] -> F.fprintf fmt "_"
  | "reducedSimpleKeysetExpression", [ [ "DEFAULT" ] ], [] ->
      F.fprintf fmt "default"
  | "reducedSimpleKeysetExpression", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_keyset_expr: ill-formed reduced simple keyset expression:\n\
            %a"
           pp_case_v value)
  | "tupleKeysetExpression", [ [ "(" ]; [ "," ]; [ ")" ] ], [ expr; exprs ] ->
      F.fprintf fmt "(%a, %a)" pp_case_v expr
        (pp_list_v ~level:0 ~sep:Comma)
        exprs
  | "tupleKeysetExpression", [ [ "(" ]; [ ")"; "PHTM_19" ] ], [ expr ] ->
      F.fprintf fmt "(%a)" pp_case_v expr
  | "tupleKeysetExpression", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_keyset_expr: ill-formed tuple keyset expression:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_keyset_expr: expected keyset expression, got %s"
           (id_of_case_v value))

and pp_syntax_targ fmt (value : value) : unit =
  match flatten_case_v value with
  | "realTypeArg", [ [ "VOID" ] ], [] -> F.fprintf fmt "void"
  | "realTypeArg", [ [ "_" ] ], [] -> F.fprintf fmt "_"
  | "realTypeArg", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_targ: ill-formed real type argument:\n%a"
           pp_case_v value)
  | "typeArg", [ [ "VOID" ] ], [] -> F.fprintf fmt "void"
  | "typeArg", [ [ "_" ] ], [] -> F.fprintf fmt "_"
  | "typeArg", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_targ: ill-formed type argument:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_targ: expected type argument, got %s"
           (id_of_case_v value))

and pp_syntax_arg fmt (value : value) : unit =
  match flatten_case_v value with
  | "argument", [ []; [ "=" ]; [] ], [ name; expr ] ->
      F.fprintf fmt "%a = (%a)" pp_case_v name pp_case_v expr
  | "argument", [ [ "_" ] ], [] -> F.fprintf fmt "_"
  | "argument", [ []; [ "="; "_" ] ], [ name ] ->
      F.fprintf fmt "%a = _" pp_case_v name
  | "argument", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_arg: ill-formed argument:\n%a" pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_arg: expected argument, got %s"
           (id_of_case_v value))

and pp_syntax_lvalue fmt (value : value) : unit =
  match flatten_case_v value with
  | "lvalue", [ [ "THIS" ] ], [] -> F.fprintf fmt "this"
  | "lvalue", [ []; [ "." ]; [] ], [ expr; name ] ->
      F.fprintf fmt "%a.%a" pp_case_v expr pp_case_v name
  | "lvalue", [ []; [ "[" ]; [ "]" ] ], [ array; index ] ->
      F.fprintf fmt "%a[%a]" pp_case_v array pp_case_v index
  | "lvalue", [ []; [ "[" ]; [ ":" ]; [ "]" ] ], [ bits; hi; lo ] ->
      F.fprintf fmt "%a[%a:%a]" pp_case_v bits pp_case_v hi pp_case_v lo
  | "lvalue", [ [ "(" ]; [ ")" ] ], [ lvalue ] ->
      F.fprintf fmt "(%a)" pp_case_v lvalue
  | "lvalue", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_lvalue: ill-formed l-value:\n%a" pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_lvalue: expected l-value, got %s"
           (id_of_case_v value))

and pp_syntax_init fmt (value : value) : unit =
  match flatten_case_v value with
  | "initializer", [ [ "=" ]; [] ], [ expr ] ->
      F.fprintf fmt " = (%a)" pp_case_v expr
  | "initializer", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_init: ill-formed initializer:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_init: expected initializer, got %s"
           (id_of_case_v value))

and pp_syntax_stmt' ~level fmt (value : value) : unit =
  let is_assign_op = function
    | "=" | "+=" | "|+|=" | "-=" | "|-|=" | "*=" | "/=" | "%=" | "<<=" | ">>="
    | "&=" | "^=" | "|=" ->
        true
    | _ -> false
  in
  match flatten_case_v value with
  | ( "assignmentOrMethodCallStatementWithoutSemicolon",
      [ []; [ "(" ]; [ ")" ] ],
      [ func; args ] ) ->
      F.fprintf fmt "%a(%a)" pp_case_v func (pp_list_v ~level:0 ~sep:Comma) args
  | ( "assignmentOrMethodCallStatementWithoutSemicolon",
      [ []; [ "<" ]; [ ">"; "(" ]; [ ")" ] ],
      [ func; targs; args ] ) ->
      F.fprintf fmt "%a<%a>(%a)" pp_case_v func
        (pp_list_v ~level:0 ~sep:Comma)
        targs
        (pp_list_v ~level:0 ~sep:Comma)
        args
  | ( "assignmentOrMethodCallStatementWithoutSemicolon",
      [ []; [ assign_op ]; [] ],
      [ lhs; rhs ] )
    when is_assign_op assign_op ->
      F.fprintf fmt "%a %s (%a)" pp_case_v lhs assign_op pp_case_v rhs
  | "assignmentOrMethodCallStatementWithoutSemicolon", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_stmt': ill-formed statement helper:\n%a"
           pp_case_v value)
  | "switchLabel", [ [ "DEFAULT" ] ], [] -> F.fprintf fmt "default"
  | "switchCase", [ []; [ ":" ]; [] ], [ label; code ] ->
      F.fprintf fmt "%a: %a" pp_case_v label (pp_syntax_stmt ~level) code
  | "switchCase", [ []; [ ":" ] ], [ label ] ->
      F.fprintf fmt "%a:" pp_case_v label
  | "forCollectionExpr", [ []; [ ".." ]; [] ], [ expr_l; expr_r ] ->
      F.fprintf fmt "(%a)..(%a)" pp_case_v expr_l pp_case_v expr_r
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_stmt': expected statement helper, got %s"
           (id_of_case_v value))

and pp_syntax_stmt ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "assignmentOrMethodCallStatement", [ []; [ ";" ] ], [ stmt' ] ->
      F.fprintf fmt "%a;" pp_case_v stmt'
  | ( "directApplication",
      [ []; [ "."; "APPLY"; "(" ]; [ ")"; ";" ] ],
      [ named_type; args ] ) ->
      F.fprintf fmt "%a.apply(%a);" pp_case_v named_type
        (pp_list_v ~level:0 ~sep:Comma)
        args
  | "conditionalStatement", [ [ "IF"; "(" ]; [ ")" ]; [] ], [ cond; tru ] ->
      F.fprintf fmt "if (%a) %a" pp_case_v cond (pp_syntax_stmt ~level) tru
  | ( "conditionalStatement",
      [ [ "IF"; "(" ]; [ ")" ]; [ "ELSE" ]; [] ],
      [ cond; tru; fls ] ) ->
      F.fprintf fmt "if (%a) %a else %a" pp_case_v cond (pp_syntax_stmt ~level)
        tru (pp_syntax_stmt ~level) fls
  | "emptyStatement", [ [ ";" ] ], [] -> F.fprintf fmt ";"
  | "blockStatement", [ []; [ "{" ]; [ "}" ] ], [ opt_annos; stmts ] ->
      F.fprintf fmt "%a{\n%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        stmts (indent level)
  | "returnStatement", [ [ "RETURN"; ";" ] ], [] -> F.fprintf fmt "return;"
  | "returnStatement", [ [ "RETURN" ]; [ ";" ] ], [ expr ] ->
      F.fprintf fmt "return %a;" pp_case_v expr
  | "breakStatement", [ [ "BREAK"; ";" ] ], [] -> F.fprintf fmt "break;"
  | "continueStatement", [ [ "CONTINUE"; ";" ] ], [] ->
      F.fprintf fmt "continue;"
  | "exitStatement", [ [ "EXIT"; ";" ] ], [] -> F.fprintf fmt "exit;"
  | ( "switchStatement",
      [ [ "SWITCH"; "(" ]; [ ")"; "{" ]; [ "}" ] ],
      [ expr; cases ] ) ->
      F.fprintf fmt "switch (%a) {\n%a\n%s}" pp_case_v expr
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        cases (indent level)
  | ( "forStatement",
      [ []; [ "FOR"; "(" ]; [ ";" ]; [ ";" ]; [ ")" ]; [] ],
      [ opt_annos; init; cond; update; body ] ) ->
      F.fprintf fmt "%afor (%a; %a; %a) %a"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos
        (pp_list_v ~level:0 ~sep:Comma)
        init pp_case_v cond
        (pp_list_v ~level:0 ~sep:Comma)
        update (pp_syntax_stmt ~level) body
  | ( "forStatement",
      [ []; [ "FOR"; "(" ]; []; [ "IN" ]; [ ")" ]; [] ],
      [ opt_annos; typ; name; collection; body ] ) ->
      F.fprintf fmt "%afor (%a %a in %a) %a"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v typ pp_case_v name pp_case_v collection
        (pp_syntax_stmt ~level) body
  | ( "forStatement",
      [ []; [ "FOR"; "(" ]; []; []; [ "IN" ]; [ ")" ]; [] ],
      [ opt_annos; opt_annos_in; typ; name; collection; body ] ) ->
      F.fprintf fmt "%afor (%a%a %a in %a) %a"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos
        (pp_opt_annos ~level:0 ~sep:SpaceSep)
        opt_annos_in pp_case_v typ pp_case_v name pp_case_v collection
        (pp_syntax_stmt ~level) body
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_stmt: expected statement, got %s"
           (id_of_case_v value))

and pp_syntax_func fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "functionPrototype",
      [ []; []; []; [ "(" ]; [ ")" ] ],
      [ typ_or_void; name; opt_tparams; params ] ) ->
      let pp_opt_tparams = pp_opt_v ~postfix:"" pp_case_v in
      F.fprintf fmt "%a %a%a(%a)" pp_case_v typ_or_void pp_case_v name
        pp_opt_tparams opt_tparams
        (pp_list_v ~level:0 ~sep:Comma)
        params
  | "functionPrototype", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_func: ill-formed function prototype:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_func: expected function prototype, got %s"
           (id_of_case_v value))

and pp_syntax_mthd ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "methodPrototype", [ []; []; [ ";" ] ], [ opt_annos; func ] ->
      F.fprintf fmt "%a%a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v func
  | "methodPrototype", [ []; [ "ABSTRACT" ]; [ ";" ] ], [ opt_annos; func ] ->
      F.fprintf fmt "%aabstract %a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v func
  | ( "methodPrototype",
      [ []; []; [ "(" ]; [ ")"; ";" ] ],
      [ opt_annos; tid; params ] ) ->
      F.fprintf fmt "%a%a(%a);"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v tid
        (pp_list_v ~level:0 ~sep:Comma)
        params
  | "methodPrototype", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_method: ill-formed methodPrototype:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_method: expected methodPrototype, got %s"
           (id_of_case_v value))

and pp_syntax_stat_or_decl ~level fmt (value : value) : unit =
  match id_of_case_v value with
  | "variableDeclaration" | "constantDeclaration" ->
      pp_syntax_decl ~level fmt value
  | "assignmentOrMethodCallStatement" | "directApplication"
  | "conditionalStatement" | "emptyStatement" | "blockStatement"
  | "returnStatement" | "breakStatement" | "continueStatement" | "exitStatement"
  | "switchStatement" | "forStatement" ->
      pp_syntax_stmt ~level fmt value
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_stat_or_decl: expected variable declaration, constant \
            declaration, or statement, got %s"
           (id_of_case_v value))

and pp_syntax_decl_or_assign_or_call_stmt fmt (value : value) : unit =
  match id_of_case_v value with
  | "variableDeclarationWithoutSemicolon" -> pp_case_v fmt value
  | "assignmentOrMethodCallStatementWithoutSemicolon" ->
      pp_syntax_stmt' ~level:0 fmt value
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_decl_or_assign_or_call_stmt: expected variable \
            declaration, assignment statement, or method call statement, got \
            %s"
           (id_of_case_v value))

and pp_syntax_obj_init ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "objInitializer", [ [ "="; "{" ]; [ "}" ] ], [ decls ] ->
      F.fprintf fmt " = {\n%a\n%s}"
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        decls (indent level)
  | "objInitializer", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_obj_init: ill-formed object initializer:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_obj_init: expected object initializer, got %s"
           (id_of_case_v value))

and pp_syntax_key fmt (value : value) : unit =
  match flatten_case_v value with
  | "keyElement", [ []; [ ":" ]; []; [ ";" ] ], [ key; match_kind; opt_annos ]
    ->
      let space = match opt_annos.it with OptV (Some _) -> " " | _ -> "" in
      F.fprintf fmt "%a : %a%s%a;" pp_case_v key pp_case_v match_kind space
        (pp_opt_annos ~level:0 ~sep:SpaceSep)
        opt_annos
  | "keyElement", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_key: ill-formed key element:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_key: expected key element, got %s"
           (id_of_case_v value))

and pp_syntax_action_ref fmt (value : value) : unit =
  match flatten_case_v value with
  | "actionRef", [ []; [ "(" ]; [ ")" ] ], [ name; args ] ->
      F.fprintf fmt "%a(%a)" pp_case_v name (pp_list_v ~level:0 ~sep:Comma) args
  | "actionRef", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_action_ref: ill-formed action ref:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_action_ref: expected action ref, got %s"
           (id_of_case_v value))

and pp_syntax_action ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "action", [ []; []; [ ";" ] ], [ opt_annos; action_ref ] ->
      F.fprintf fmt "%a%a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v action_ref
  | "action", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_action: ill-formed action:\n%a" pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_action: expected action, got %s"
           (id_of_case_v value))

and pp_syntax_entry_prio fmt (value : value) : unit =
  match flatten_case_v value with
  | "entryPriority", [ [ "PRIORITY"; "=" ]; [ ":" ] ], [ num ] ->
      F.fprintf fmt "priority = %a:" pp_case_v num
  | "entryPriority", [ [ "PRIORITY"; "="; "(" ]; [ ")"; ":" ] ], [ expr ] ->
      F.fprintf fmt "priority = (%a):" pp_case_v expr
  | "entryPriority", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_entry_prio: ill-formed entry priority:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_entry_prio: expected entry priority, got %s"
           (id_of_case_v value))

and pp_syntax_entry fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "entry",
      [ []; []; []; [ ":" ]; []; [ ";" ] ],
      [ opt_const; prio; keyset; action; opt_annos ] ) ->
      let space = match opt_annos.it with OptV (Some _) -> " " | _ -> "" in
      F.fprintf fmt "%a%a %a : %a%s%a;"
        (pp_opt_v ~postfix:" " pp_case_v)
        opt_const pp_case_v prio pp_case_v keyset pp_case_v action space
        (pp_opt_annos ~level:0 ~sep:SpaceSep)
        opt_annos
  | ( "entry",
      [ []; []; [ ":" ]; []; [ ";" ] ],
      [ opt_const; keyset; action; opt_annos ] ) ->
      let space = match opt_annos.it with OptV (Some _) -> " " | _ -> "" in
      F.fprintf fmt "%a%a : %a%s%a;"
        (pp_opt_v ~postfix:" " pp_case_v)
        opt_const pp_case_v keyset pp_case_v action space
        (pp_opt_annos ~level:0 ~sep:SpaceSep)
        opt_annos
  | "entry", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_entry: ill-formed entry:\n%a" pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_entry: expected entry, got %s"
           (id_of_case_v value))

and pp_syntax_table_prop ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "tableProperty", [ [ "KEY"; "="; "{" ]; [ "}" ] ], [ keys ] ->
      F.fprintf fmt "key = {\n%a\n%s}"
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        keys (indent level)
  | "tableProperty", [ [ "ACTIONS"; "="; "{" ]; [ "}" ] ], [ actions ] ->
      F.fprintf fmt "actions = {\n%a\n%s}"
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        actions (indent level)
  | ( "tableProperty",
      [ []; []; [ "ENTRIES"; "="; "{" ]; [ "}" ] ],
      [ opt_annos; opt_const; entries ] ) ->
      F.fprintf fmt "%a%aentries = {\n%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos
        (pp_opt_v ~postfix:" " pp_case_v)
        opt_const
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        entries (indent level)
  | ( "tableProperty",
      [ []; []; []; []; [ ";" ] ],
      [ opt_annos; opt_const; name; init ] ) ->
      F.fprintf fmt "%a%a%a%a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos
        (pp_opt_v ~postfix:" " pp_case_v)
        opt_const pp_case_v name pp_case_v init
  | "tableProperty", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_table_prop: ill-formed table property:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_table_prop: expected table property, got %s"
           (id_of_case_v value))

and pp_syntax_ctrl_typ_decl ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "controlTypeDeclaration",
      [ []; [ "CONTROL" ]; []; [ "(" ]; [ ")" ] ],
      [ opt_annos; name; tparams; params ] ) ->
      F.fprintf fmt "%acontrol %a%a(%a)"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_opt_v ~postfix:"" pp_case_v)
        tparams
        (pp_list_v ~level:0 ~sep:Comma)
        params
  | "controlTypeDeclaration", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_ctrl_typ_decl: ill-formed control type declaration:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_ctrl_typ_decl: expected control type declaration, got %s"
           (id_of_case_v value))

and pp_syntax_select_case fmt (value : value) : unit =
  match flatten_case_v value with
  | "selectCase", [ []; [ ":" ]; [ ";" ] ], [ keyset_expr; name ] ->
      F.fprintf fmt "%a: %a;" pp_case_v keyset_expr pp_case_v name
  | "selectCase", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_select_case: ill-formed select case:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_select_case: expected select case, got %s"
           (id_of_case_v value))

and pp_syntax_select_expr ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "selectExpression",
      [ [ "SELECT"; "(" ]; [ ")"; "{" ]; [ "}" ] ],
      [ exprs; cases ] ) ->
      F.fprintf fmt "select (%a) {\n%a\n%s}"
        (pp_list_v ~level:0 ~sep:Comma)
        exprs
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        cases (indent level)
  | "selectExpression", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_select_expr: ill-formed select expression:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_select_expr: expected select expression, got %s"
           (id_of_case_v value))

and pp_syntax_state_expr ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "stateExpression", [ []; [ ";" ] ], [ name ] ->
      F.fprintf fmt "%a;" pp_case_v name
  | "stateExpression", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_state_expr: ill-formed state expression:\n%a"
           pp_case_v value)
  | "selectExpression", _, _ -> pp_syntax_select_expr ~level fmt value
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_state_expr: expected state expression, got %s"
           (id_of_case_v value))

and pp_syntax_trans_stmt ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "transitionStatement", [ [ "SPEC_BUG" ] ], [] -> ()
  | "transitionStatement", [ [ "TRANSITION" ]; [] ], [ state_expr ] ->
      F.fprintf fmt "transition %a" (pp_syntax_state_expr ~level) state_expr
  | "transitionStatement", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_trans_stmt: ill-formed transition statement:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_trans_stmt: expected transition statement, got %s"
           (id_of_case_v value))

and pp_syntax_parser_stmt ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "assignmentOrMethodCallStatement", _, _
  | "directApplication", _, _
  | "emptyStatement", _, _
  | "conditionalStatement", _, _ ->
      pp_syntax_stmt ~level fmt value
  | "variableDeclaration", _, _ | "constantDeclaration", _, _ ->
      pp_syntax_decl ~level fmt value
  | "parserBlockStatement", [ []; [ "{" ]; [ "}" ] ], [ opt_annos; stmts ] ->
      F.fprintf fmt "%a{\n%a\n%s}"
        (pp_opt_annos ~level ~sep:SpaceSep)
        opt_annos
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        stmts (indent level)
  | "parserBlockStatement", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_parser_stmt: ill-formed parser block statement:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_parser_stmt: expected parser statement, got %s"
           (id_of_case_v value))

and pp_syntax_parser_state ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "parserState",
      [ []; [ "STATE" ]; [ "{" ]; []; [ "}" ] ],
      [ opt_annos; name; stmts; trans ] ) ->
      F.fprintf fmt "%astate %a {\n%a\n%s%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        stmts
        (indent (level + 1))
        (pp_syntax_trans_stmt ~level:(level + 1))
        trans (indent level)
  | "parserState", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_parser_state: ill-formed parser state:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_parser_state: expected parser state, got %s"
           (id_of_case_v value))

and pp_syntax_parser_type_decl ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "parserTypeDeclaration",
      [ []; [ "PARSER" ]; []; [ "(" ]; [ ")" ] ],
      [ opt_annos; name; opt_tparams; params ] ) ->
      F.fprintf fmt "%aparser %a%a(%a)"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_tparams
        (pp_list_v ~level:0 ~sep:Comma)
        params
  | "parserTypeDeclaration", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_parser_type_decl: ill-formed parser type declaration:\n\
            %a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_parser_type_decl: expected parser type declaration, got \
            %s"
           (id_of_case_v value))

and pp_syntax_pckg_type_decl ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "packageTypeDeclaration",
      [ []; [ "PACKAGE" ]; []; [ "(" ]; [ ")" ] ],
      [ opt_annos; name; opt_tparams; params ] ) ->
      F.fprintf fmt "%apackage %a%a(%a)"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_tparams
        (pp_list_v ~level:0 ~sep:Comma)
        params
  | "packageTypeDeclaration", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_pckg_type_decl: ill-formed package type declaration:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_pckg_type_decl: expected package type declaration, got \
            %s"
           (id_of_case_v value))

and pp_syntax_decl ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | ( "constantDeclaration",
      [ []; [ "CONST" ]; []; []; [ ";" ] ],
      [ opt_annos; type_ref; name; init ] ) ->
      F.fprintf fmt "%aconst %a %a%a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v type_ref pp_case_v name pp_case_v init
  | "variableDeclaration", [ []; [ ";" ] ], [ decl ] ->
      F.fprintf fmt "%a;" pp_case_v decl
  | ( "matchKindDeclaration",
      [ [ "MATCH_KIND"; "{" ]; []; [ "}" ] ],
      [ ids; comma ] ) ->
      F.fprintf fmt "match_kind {\n%a%a\n%s}"
        (pp_list_v ~level:(level + 1) ~sep:CommaNl)
        ids
        (pp_opt_v ~postfix:"" pp_case_v)
        comma (indent level)
  | "errorDeclaration", [ [ "ERROR"; "{" ]; [ "}" ] ], [ ids ] ->
      F.fprintf fmt "error {\n%a\n%s}"
        (pp_list_v ~level:(level + 1) ~sep:CommaNl)
        ids (indent level)
  | ( "externDeclaration",
      [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
      [ opt_annos; name; opt_tparams; mthds ] ) ->
      F.fprintf fmt "%aextern %a%a {\n%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_tparams
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        mthds (indent level)
  | "externDeclaration", [ []; [ "EXTERN" ]; [ ";" ] ], [ opt_annos; func ] ->
      F.fprintf fmt "%aextern %a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v func
  | "functionDeclaration", [ []; []; []; [] ], [ opt_annos; func; body ] ->
      F.fprintf fmt "%a%a %a"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v func (pp_syntax_stmt ~level) body
  | ( "instantiation",
      [ []; []; [ "(" ]; [ ")" ]; [ ";" ] ],
      [ opt_annos; type_ref; args; name ] ) ->
      F.fprintf fmt "%a%a(%a) %a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v type_ref
        (pp_list_v ~level:0 ~sep:Comma)
        args pp_case_v name
  | ( "instantiation",
      [ []; []; [ "(" ]; [ ")" ]; []; [ ";" ] ],
      [ opt_annos; type_ref; args; name; init ] ) ->
      F.fprintf fmt "%a%a(%a) %a%a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v type_ref
        (pp_list_v ~level:0 ~sep:Comma)
        args pp_case_v name
        (pp_syntax_obj_init ~level)
        init
  | ( "actionDeclaration",
      [ []; [ "ACTION" ]; [ "(" ]; [ ")" ]; [] ],
      [ opt_annos; name; params; body ] ) ->
      F.fprintf fmt "%aaction %a(%a) %a"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_list_v ~level:0 ~sep:Comma)
        params (pp_syntax_stmt ~level) body
  | ( "tableDeclaration",
      [ []; [ "TABLE" ]; [ "{" ]; [ "}" ] ],
      [ opt_annos; name; props ] ) ->
      F.fprintf fmt "%atable %a {\n%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        props (indent level)
  | ( "controlDeclaration",
      [ []; []; [ "{" ]; [ "APPLY" ]; [ "}" ] ],
      [ control_type_decl; opt_constructor_params; locals; apply_body ] ) ->
      F.fprintf fmt "%a%a {\n%a\n%sapply %a\n%s}"
        (pp_syntax_ctrl_typ_decl ~level)
        control_type_decl
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_constructor_params
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        locals
        (indent (level + 1))
        (pp_syntax_stmt ~level:(level + 1))
        apply_body (indent level)
  | ( "valueSetDeclaration",
      [ []; [ "VALUESET"; "<" ]; [ ">"; "(" ]; [ ")" ]; [ ";" ] ],
      [ opt_annos; base_type; size; name ] ) ->
      F.fprintf fmt "%avalue_set<%a>(%a) %a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v base_type pp_case_v size pp_case_v name
  | ( "valueSetDeclaration",
      [ []; [ "VALUESET"; "<" ]; [ ">"; "(" ]; [ ")" ]; [ ";"; "PHTM_17" ] ],
      [ opt_annos; tuple; size; name ] ) ->
      F.fprintf fmt "%avalue_set<%a>(%a) %a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v tuple pp_case_v size pp_case_v name
  | ( "valueSetDeclaration",
      [ []; [ "VALUESET"; "<" ]; [ ">"; "(" ]; [ ")" ]; [ ";"; "PHTM_18" ] ],
      [ opt_annos; type_name; size; name ] ) ->
      F.fprintf fmt "%avalue_set<%a>(%a) %a;"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v type_name pp_case_v size pp_case_v name
  | ( "parserDeclaration",
      [ []; []; [ "{" ]; []; [ "}" ] ],
      [ decl; params; locals; states ] ) ->
      F.fprintf fmt "%a%a {\n%a\n%a\n%s}"
        (pp_syntax_parser_type_decl ~level)
        decl
        (pp_opt_v ~postfix:"" pp_case_v)
        params
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        locals
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        states (indent level)
  | ( "enumDeclaration",
      [ []; [ "ENUM" ]; [ "{" ]; []; [ "}" ] ],
      [ opt_annos; name; ids; opt_comma ] ) ->
      F.fprintf fmt "%aenum %a {\n%a%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_list_v ~level:(level + 1) ~sep:CommaNl)
        ids
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_comma (indent level)
  | ( "enumDeclaration",
      [ []; [ "ENUM" ]; []; [ "{" ]; []; [ "}" ] ],
      [ opt_annos; type_ref; name; ids; opt_comma ] ) ->
      F.fprintf fmt "%aenum %a %a {\n%a%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v type_ref pp_case_v name
        (pp_list_v ~level:(level + 1) ~sep:CommaNl)
        ids
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_comma (indent level)
  | ( "headerUnionDeclaration",
      [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
      [ opt_annos; name; tparams; fields ] ) ->
      F.fprintf fmt "%aheader_union %a%a {\n%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_opt_v ~postfix:"" pp_case_v)
        tparams
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        fields (indent level)
  | ( "structTypeDeclaration",
      [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
      [ opt_annos; name; tparams; fields ] ) ->
      F.fprintf fmt "%astruct %a%a {\n%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_opt_v ~postfix:"" pp_case_v)
        tparams
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        fields (indent level)
  | ( "headerTypeDeclaration",
      [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
      [ opt_annos; name; tparams; fields ] ) ->
      F.fprintf fmt "%aheader %a%a {\n%a\n%s}"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v name
        (pp_opt_v ~postfix:"" pp_case_v)
        tparams
        (pp_list_v ~level:(level + 1) ~sep:Nl)
        fields (indent level)
  | ( "typedefDeclaration",
      [ []; [ "TYPEDEF" ]; []; [] ],
      [ opt_annos; type_ref; name ] ) ->
      F.fprintf fmt "%atypedef %a %a"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v type_ref pp_case_v name
  | ( "typedefDeclaration",
      [ []; [ "TYPEDEF" ]; []; [ "PHTM_12" ] ],
      [ opt_annos; derived; name ] ) ->
      F.fprintf fmt "%atypedef %a %a"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos (pp_syntax_decl ~level:0) derived pp_case_v name
  | ( "typedefDeclaration",
      [ []; [ "TYPE" ]; []; [] ],
      [ opt_annos; type_ref; name ] ) ->
      F.fprintf fmt "%atype %a %a"
        (pp_opt_annos ~level ~sep:Nl)
        opt_annos pp_case_v type_ref pp_case_v name
  | "typeDeclaration", [ []; [ ";" ] ], [ typedef ] ->
      F.fprintf fmt "%a;" (pp_syntax_decl ~level) typedef
  | "typeDeclaration", [ []; [ ";"; "PHTM_13" ] ], [ parser_type_decl ] ->
      F.fprintf fmt "%a;" (pp_syntax_parser_type_decl ~level) parser_type_decl
  | "typeDeclaration", [ []; [ ";"; "PHTM_14" ] ], [ ctrl_type_decl ] ->
      F.fprintf fmt "%a;" (pp_syntax_ctrl_typ_decl ~level) ctrl_type_decl
  | "typeDeclaration", [ []; [ ";"; "PHTM_15" ] ], [ pckg_type_decl ] ->
      F.fprintf fmt "%a;" (pp_syntax_pckg_type_decl ~level) pckg_type_decl
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_decl: expected declaration, got %s"
           (id_of_case_v value))

and pp_syntax_anno_token fmt (value : value) : unit =
  match flatten_case_v value with
  | "annotationToken", [ [ "UNEXPECTED_TOKEN" ] ], [] ->
      F.fprintf fmt "unexpected_token"
  | "annotationToken", [ [ "ABSTRACT" ] ], [] -> F.fprintf fmt "abstract"
  | "annotationToken", [ [ "ACTION" ] ], [] -> F.fprintf fmt "action"
  | "annotationToken", [ [ "ACTIONS" ] ], [] -> F.fprintf fmt "actions"
  | "annotationToken", [ [ "APPLY" ] ], [] -> F.fprintf fmt "apply"
  | "annotationToken", [ [ "BOOL" ] ], [] -> F.fprintf fmt "bool"
  | "annotationToken", [ [ "BIT" ] ], [] -> F.fprintf fmt "bit"
  | "annotationToken", [ [ "BREAK" ] ], [] -> F.fprintf fmt "break"
  | "annotationToken", [ [ "CONST" ] ], [] -> F.fprintf fmt "const"
  | "annotationToken", [ [ "CONTINUE" ] ], [] -> F.fprintf fmt "continue"
  | "annotationToken", [ [ "CONTROL" ] ], [] -> F.fprintf fmt "control"
  | "annotationToken", [ [ "DEFAULT" ] ], [] -> F.fprintf fmt "default"
  | "annotationToken", [ [ "ELSE" ] ], [] -> F.fprintf fmt "else"
  | "annotationToken", [ [ "ENTRIES" ] ], [] -> F.fprintf fmt "entries"
  | "annotationToken", [ [ "ENUM" ] ], [] -> F.fprintf fmt "enum"
  | "annotationToken", [ [ "ERROR" ] ], [] -> F.fprintf fmt "error"
  | "annotationToken", [ [ "EXIT" ] ], [] -> F.fprintf fmt "exit"
  | "annotationToken", [ [ "EXTERN" ] ], [] -> F.fprintf fmt "extern"
  | "annotationToken", [ [ "FALSE" ] ], [] -> F.fprintf fmt "false"
  | "annotationToken", [ [ "FOR" ] ], [] -> F.fprintf fmt "for"
  | "annotationToken", [ [ "HEADER" ] ], [] -> F.fprintf fmt "header"
  | "annotationToken", [ [ "HEADER_UNION" ] ], [] ->
      F.fprintf fmt "header_union"
  | "annotationToken", [ [ "IF" ] ], [] -> F.fprintf fmt "if"
  | "annotationToken", [ [ "IN" ] ], [] -> F.fprintf fmt "in"
  | "annotationToken", [ [ "INOUT" ] ], [] -> F.fprintf fmt "inout"
  | "annotationToken", [ [ "INT" ] ], [] -> F.fprintf fmt "int"
  | "annotationToken", [ [ "KEY" ] ], [] -> F.fprintf fmt "key"
  | "annotationToken", [ [ "MATCH_KIND" ] ], [] -> F.fprintf fmt "match_kind"
  | "annotationToken", [ [ "TYPE" ] ], [] -> F.fprintf fmt "type"
  | "annotationToken", [ [ "OUT" ] ], [] -> F.fprintf fmt "out"
  | "annotationToken", [ [ "PARSER" ] ], [] -> F.fprintf fmt "parser"
  | "annotationToken", [ [ "PACKAGE" ] ], [] -> F.fprintf fmt "package"
  | "annotationToken", [ [ "PRAGMA" ] ], [] -> F.fprintf fmt "pragma"
  | "annotationToken", [ [ "RETURN" ] ], [] -> F.fprintf fmt "return"
  | "annotationToken", [ [ "SELECT" ] ], [] -> F.fprintf fmt "select"
  | "annotationToken", [ [ "STATE" ] ], [] -> F.fprintf fmt "state"
  | "annotationToken", [ [ "STRING" ] ], [] -> F.fprintf fmt "string"
  | "annotationToken", [ [ "STRUCT" ] ], [] -> F.fprintf fmt "struct"
  | "annotationToken", [ [ "SWITCH" ] ], [] -> F.fprintf fmt "switch"
  | "annotationToken", [ [ "TABLE" ] ], [] -> F.fprintf fmt "table"
  | "annotationToken", [ [ "THIS" ] ], [] -> F.fprintf fmt "this"
  | "annotationToken", [ [ "TRANSITION" ] ], [] -> F.fprintf fmt "transition"
  | "annotationToken", [ [ "TRUE" ] ], [] -> F.fprintf fmt "true"
  | "annotationToken", [ [ "TUPLE" ] ], [] -> F.fprintf fmt "tuple"
  | "annotationToken", [ [ "TYPEDEF" ] ], [] -> F.fprintf fmt "typedef"
  | "annotationToken", [ [ "VARBIT" ] ], [] -> F.fprintf fmt "varbit"
  | "annotationToken", [ [ "VALUESET" ] ], [] -> F.fprintf fmt "value_set"
  | "annotationToken", [ [ "LIST" ] ], [] -> F.fprintf fmt "list"
  | "annotationToken", [ [ "VOID" ] ], [] -> F.fprintf fmt "void"
  | "annotationToken", [ [ "_" ] ], [] -> F.fprintf fmt "_"
  | "annotationToken", [ [ "&&&" ] ], [] -> F.fprintf fmt "&&&"
  | "annotationToken", [ [ ".." ] ], [] -> F.fprintf fmt ".."
  | "annotationToken", [ [ "<<" ] ], [] -> F.fprintf fmt "<<"
  | "annotationToken", [ [ "&&" ] ], [] -> F.fprintf fmt "&&"
  | "annotationToken", [ [ "||" ] ], [] -> F.fprintf fmt "||"
  | "annotationToken", [ [ "==" ] ], [] -> F.fprintf fmt "=="
  | "annotationToken", [ [ "!=" ] ], [] -> F.fprintf fmt "!="
  | "annotationToken", [ [ ">=" ] ], [] -> F.fprintf fmt ">="
  | "annotationToken", [ [ "<=" ] ], [] -> F.fprintf fmt "<="
  | "annotationToken", [ [ "++" ] ], [] -> F.fprintf fmt "++"
  | "annotationToken", [ [ "+" ] ], [] -> F.fprintf fmt "+"
  | "annotationToken", [ [ "|+|" ] ], [] -> F.fprintf fmt "|+|"
  | "annotationToken", [ [ "-" ] ], [] -> F.fprintf fmt "-"
  | "annotationToken", [ [ "|-|" ] ], [] -> F.fprintf fmt "|-|"
  | "annotationToken", [ [ "*" ] ], [] -> F.fprintf fmt "*"
  | "annotationToken", [ [ "/" ] ], [] -> F.fprintf fmt "/"
  | "annotationToken", [ [ "%" ] ], [] -> F.fprintf fmt "%%"
  | "annotationToken", [ [ "|" ] ], [] -> F.fprintf fmt "|"
  | "annotationToken", [ [ "&" ] ], [] -> F.fprintf fmt "&"
  | "annotationToken", [ [ "^" ] ], [] -> F.fprintf fmt "^"
  | "annotationToken", [ [ "~" ] ], [] -> F.fprintf fmt "~"
  | "annotationToken", [ [ "[" ] ], [] -> F.fprintf fmt "["
  | "annotationToken", [ [ "]" ] ], [] -> F.fprintf fmt "]"
  | "annotationToken", [ [ "{" ] ], [] -> F.fprintf fmt "{"
  | "annotationToken", [ [ "}" ] ], [] -> F.fprintf fmt "}"
  | "annotationToken", [ [ "<" ] ], [] -> F.fprintf fmt "<"
  | "annotationToken", [ [ ">" ] ], [] -> F.fprintf fmt ">"
  | "annotationToken", [ [ "!" ] ], [] -> F.fprintf fmt "!"
  | "annotationToken", [ [ ":" ] ], [] -> F.fprintf fmt ":"
  | "annotationToken", [ [ ","; "PHTM_21" ] ], [] -> F.fprintf fmt ","
  | "annotationToken", [ [ "?" ] ], [] -> F.fprintf fmt "?"
  | "annotationToken", [ [ "." ] ], [] -> F.fprintf fmt "."
  | "annotationToken", [ [ "=" ] ], [] -> F.fprintf fmt "="
  | "annotationToken", [ [ ";" ] ], [] -> F.fprintf fmt ";"
  | "annotationToken", [ [ "@" ] ], [] -> F.fprintf fmt "@"
  | "annotationToken", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_anno_token: ill-formed annotation token:\n%a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_anno_token: expected annotation token, got %s"
           (id_of_case_v value))

and pp_syntax_struct_anno_body fmt (value : value) : unit =
  match flatten_case_v value with
  | "structuredAnnotationBody", [ []; []; [] ], [ exprs; opt_comma ] ->
      F.fprintf fmt "%a%a"
        (pp_list_v ~level:0 ~sep:Comma)
        exprs
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_comma
  | "structuredAnnotationBody", [ []; []; [ "PHTM_20" ] ], [ kvs; opt_comma ] ->
      F.fprintf fmt "%a%a"
        (pp_list_v ~level:0 ~sep:Comma)
        kvs
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_comma
  | "structuredAnnotationBody", _, _ ->
      failwith
        (F.asprintf
           "@pp_syntax_struct_anno_body: ill-formed structured annotation body:\n\
            %a"
           pp_case_v value)
  | _ ->
      failwith
        (Printf.sprintf
           "@pp_syntax_struct_anno_body: expected structured annotation body, \
            got %s"
           (id_of_case_v value))

and pp_syntax_anno fmt (value : value) : unit =
  match flatten_case_v value with
  | "annotation", [ [ "@" ]; [] ], [ name ] ->
      F.fprintf fmt "@%a" pp_case_v name
  | "annotation", [ [ "@" ]; [ "(" ]; [ ")" ] ], [ name; body ] ->
      F.fprintf fmt "@%a(%a)" pp_case_v name
        (pp_list_v ~level:0 ~sep:Comma)
        body
  | "annotation", [ [ "@" ]; [ "[" ]; [ "]" ] ], [ name; body ] ->
      F.fprintf fmt "@%a[%a]" pp_case_v name pp_case_v body
  | "annotation", [ [ "PRAGMA" ]; []; [ "PRAGMA_END" ] ], [ name; body ] ->
      F.fprintf fmt "@pragma %a %a" pp_case_v name
        (pp_list_v ~level:0 ~sep:SpaceSep)
        body
  | "annotation", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_anno: ill-formed annotation:\n%a" pp_case_v
           value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_anno: expected annotation, got %s"
           (id_of_case_v value))

and pp_opt_annos ?(level = 0) ~sep fmt (value : value) : unit =
  let postfix = if is_nl sep then F.sprintf "\n%s" (indent level) else " " in
  pp_opt_v ~postfix (pp_list_v ~level ~sep) fmt value

and pp_case_v' fmt (value : value) : unit =
  match flatten_case_v value with
  (* Misc *)
  | "trailingComma", [ [ ","; "PHTM_0" ] ], [] -> F.fprintf fmt ","
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
      F.fprintf fmt "\"%a\"" pp_text_v value_text
  (* Names *)
  | "dotPrefix", [ [ "." ] ], [] -> F.fprintf fmt "."
  (* Type references *)
  | "typeOrVoid", [ [ "VOID" ] ], [] -> F.fprintf fmt "void"
  (* Key value pair *)
  | "kvPair", [ []; [ "=" ]; [] ], [ key; value ] ->
      F.fprintf fmt "%a = %a" pp_case_v key pp_case_v value
  (* Declarations *)
  | ( "variableDeclarationWithoutSemicolon",
      [ []; []; []; []; [] ],
      [ opt_annos; type_ref; name; opt_init ] ) ->
      F.fprintf fmt "%a%a %a%a"
        (pp_opt_annos ~level:0 ~sep:SpaceSep)
        opt_annos pp_case_v type_ref pp_case_v name
        (pp_opt_v ~postfix:"" pp_case_v)
        opt_init
  | "specifiedIdentifier", [ []; []; [] ], [ name; init ] ->
      F.fprintf fmt "%a%a" pp_case_v name pp_case_v init
  | "structField", [ []; []; []; [ ";" ] ], [ opt_annos; type_ref; name ] ->
      F.fprintf fmt "%a%a %a;"
        (pp_opt_annos ~level:0 ~sep:SpaceSep)
        opt_annos pp_case_v type_ref pp_case_v name
  (* Annotations *)
  | "simpleAnnotation", [ [ "(" ]; [ ")" ] ], [ body ] ->
      F.fprintf fmt "(%a)" (pp_list_v ~level:0 ~sep:Comma) body
  | _ -> pp_default_case_v fmt value

and pp_case_v fmt (value : value) : unit =
  match id_of_case_v value with
  | "constantDeclaration" | "variableDeclaration" | "errorDeclaration"
  | "matchKindDeclaration" | "externDeclaration" | "instantiation"
  | "functionDeclaration" | "actionDeclaration" | "parserDeclaration"
  | "controlDeclaration" | "valueSetDeclaration" | "headerTypeDeclaration"
  | "headerUnionDeclaration" | "structTypeDeclaration" | "enumDeclaration"
  | "typedefDeclaration" | "typeDeclaration" ->
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
  | "simpleKeysetExpression" | "reducedSimpleKeysetExpression"
  | "tupleKeysetExpression" ->
      pp_syntax_keyset_expr fmt value
  | "realTypeArg" | "typeArg" -> pp_syntax_targ fmt value
  | "argument" -> pp_syntax_arg fmt value
  | "lvalue" -> pp_syntax_lvalue fmt value
  | "initializer" -> pp_syntax_init fmt value
  | "assignmentOrMethodCallStatementWithoutSemicolon" | "switchLabel"
  | "switchCase" | "forCollectionExpr" ->
      pp_syntax_stmt' ~level:0 fmt value
  | "assignmentOrMethodCallStatement" | "directApplication"
  | "conditionalStatement" | "emptyStatement" | "blockStatement"
  | "returnStatement" | "breakStatement" | "continueStatement" | "exitStatement"
  | "switchStatement" | "forStatement" ->
      pp_syntax_stmt ~level:0 fmt value
  | "functionPrototype" -> pp_syntax_func fmt value
  | "methodPrototype" -> pp_syntax_mthd ~level:0 fmt value
  | "keyElement" -> pp_syntax_key fmt value
  | "actionRef" -> pp_syntax_action_ref fmt value
  | "action" -> pp_syntax_action ~level:0 fmt value
  | "entryPriority" -> pp_syntax_entry_prio fmt value
  | "entry" -> pp_syntax_entry fmt value
  | "tableProperty" -> pp_syntax_table_prop ~level:0 fmt value
  | "controlTypeDeclaration" -> pp_syntax_ctrl_typ_decl ~level:0 fmt value
  | "selectCase" -> pp_syntax_select_case fmt value
  | "selectExpression" -> pp_syntax_select_expr ~level:0 fmt value
  | "stateExpression" -> pp_syntax_state_expr ~level:0 fmt value
  | "transitionStatement" -> pp_syntax_trans_stmt ~level:0 fmt value
  | "parserBlockStatement" -> pp_syntax_parser_stmt ~level:0 fmt value
  | "parserState" -> pp_syntax_parser_state ~level:0 fmt value
  | "parserTypeDeclaration" -> pp_syntax_parser_type_decl ~level:0 fmt value
  | "packageTypeDeclaration" -> pp_syntax_pckg_type_decl ~level:0 fmt value
  | "annotationToken" -> pp_syntax_anno_token fmt value
  | "structuredAnnotationBody" -> pp_syntax_struct_anno_body fmt value
  | "annotation" -> pp_syntax_anno fmt value
  | _ -> pp_case_v' fmt value
