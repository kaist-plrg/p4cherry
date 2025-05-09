module F = Format
module L = Lang.Ast
module P = Lang.Pp
open Ast
open Util.Pp
open Util.Source

(* Numbers *)

let pp_num fmt num = P.pp_num fmt num

(* Texts *)

let pp_text fmt text = P.pp_text fmt text

(* Identifiers *)

let pp_id' fmt id = P.pp_id' fmt id
let pp_id fmt id = P.pp_id fmt id

(* Variables (scoped identifiers) *)

let pp_var fmt var = P.pp_var fmt var

(* Members *)

let pp_member' fmt member = P.pp_member' fmt member
let pp_member fmt member = P.pp_member fmt member
let pp_members ?(level = 0) fmt members = P.pp_members ~level fmt members

(* Unary operators *)

let pp_unop fmt unop = P.pp_unop fmt unop

(* Binary operators *)

let pp_binop fmt binop = P.pp_binop fmt binop

(* Directions *)

let pp_dir fmt dir = P.pp_dir fmt dir

(* Types *)

let rec pp_typ' ?(level = 0) fmt typ =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ | VarT _ ->
      Pp.pp_typ' ~level fmt typ
  | SpecT ((_, _, ListT _), typs) ->
      F.fprintf fmt "list<%a>"
        (pp_list (pp_typ' ~level:(level + 1)) ~sep:Comma)
        typs
  | SpecT ((_, _, TupleT _), typs) ->
      F.fprintf fmt "tuple<%a>"
        (pp_list (pp_typ' ~level:(level + 1)) ~sep:Comma)
        typs
  | SpecT ((_, _, StackT (_, size)), typs) ->
      F.fprintf fmt "%a[%a]"
        (pp_list (pp_typ' ~level:(level + 1)) ~sep:Comma)
        typs Bigint.pp size
  | SpecT (tdp, typs) -> (
      let _, _, typ = tdp in
      match typs with
      | [] -> F.fprintf fmt "%a" (pp_typ' ~level:(level + 1)) typ
      | _ ->
          F.fprintf fmt "%a<%a>"
            (pp_typ' ~level:(level + 1))
            typ
            (pp_list (pp_typ' ~level:(level + 1)) ~sep:Comma)
            typs)
  | DefT (id, _) | NewT (id, _) | EnumT (id, _) | SEnumT (id, _, _) ->
      F.fprintf fmt "%a" pp_id' id
  | ListT _ | TupleT _ | StackT _ -> assert false
  | StructT (id, _)
  | HeaderT (id, _)
  | UnionT (id, _)
  | ExternT (id, _)
  | ParserT (id, _)
  | ControlT (id, _)
  | PackageT (id, _)
  | TableT (id, _) ->
      F.fprintf fmt "%a" pp_id' id
  | AnyT -> F.fprintf fmt "_"
  | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
  | RecordDefaultT _ | DefaultT | InvalidT ->
      assert false
  | SetT _ -> Pp.pp_typ' ~level fmt typ
  | StateT -> assert false

let pp_typ ?(level = 0) fmt typ = pp_typ' ~level fmt typ.it

(* Values *)

let rec pp_value ?(level = 0) fmt value = pp_expr' ~level fmt value.note

(* Type parameters *)

and pp_tparams fmt tparams = P.pp_tparams fmt tparams

(* Parameters *)

and pp_param' ?(level = 0) fmt param' =
  let id, dir, typ, value_default, _annos = param' in
  match value_default with
  | Some value_default ->
      if dir.it = L.No then
        F.fprintf fmt "%a %a = %a"
          (pp_typ ~level:(level + 1))
          typ pp_id id
          (pp_value ~level:(level + 1))
          value_default
      else
        F.fprintf fmt "%a %a %a = %a" pp_dir dir
          (pp_typ ~level:(level + 1))
          typ pp_id id
          (pp_value ~level:(level + 1))
          value_default
  | None ->
      if dir.it = L.No then
        F.fprintf fmt "%a %a" (pp_typ ~level:(level + 1)) typ pp_id id
      else
        F.fprintf fmt "%a %a %a" pp_dir dir
          (pp_typ ~level:(level + 1))
          typ pp_id id

and pp_param ?(level = 0) fmt param = pp_param' ~level fmt param.it

and pp_params ?(level = 0) fmt params =
  match params with
  | [] -> F.pp_print_string fmt "()"
  | [ param ] -> F.fprintf fmt "(%a)" (pp_param ~level) param
  | params ->
      F.fprintf fmt "(%a)" (pp_list ~level (pp_param ~level) ~sep:Comma) params

(* Constructor parameters *)

and pp_cparams ?(level = 0) fmt cparams = pp_params ~level fmt cparams

(* Type arguments *)

and pp_targ ?(level = 0) fmt targ = pp_typ ~level fmt targ

and pp_targs ?(level = 0) fmt targs =
  match targs with
  | [] -> ()
  | targs ->
      F.fprintf fmt "<%a>"
        (pp_list ~level:(level + 1) (pp_targ ~level:(level + 1)) ~sep:Comma)
        targs

(* Arguments *)

and pp_args fmt args = P.pp_args pp_expr fmt args

(* Expressions *)

and pp_expr' ?(level = 0) fmt expr' =
  match expr' with
  | ValueE { value } -> pp_value fmt value
  | BoolE { boolean } -> F.fprintf fmt "%b" boolean
  | StrE { text } -> F.fprintf fmt "\"%a\"" pp_text text
  | NumE { num } -> pp_num fmt num
  | VarE { var } -> pp_var fmt var
  | SeqE { exprs } ->
      F.fprintf fmt "{ %a }"
        (pp_list (pp_expr ~level:(level + 1)) ~sep:Comma)
        exprs
  | SeqDefaultE { exprs } ->
      if exprs = [] then F.pp_print_string fmt "{ ... }"
      else
        F.fprintf fmt "{ %a, ... }"
          (pp_list (pp_expr ~level:(level + 1)) ~sep:Comma)
          exprs
  | RecordE { fields } ->
      F.fprintf fmt "{ %a }"
        (pp_pairs pp_member (pp_expr ~level:(level + 1)) ~rel:Eq ~sep:Comma)
        fields
  | RecordDefaultE { fields } ->
      F.fprintf fmt "{ %a, ... }"
        (pp_pairs pp_member (pp_expr ~level:(level + 1)) ~rel:Eq ~sep:Comma)
        fields
  | DefaultE -> F.pp_print_string fmt "..."
  | InvalidE -> F.pp_print_string fmt "{#}"
  | UnE { unop; expr } ->
      F.fprintf fmt "%a%a" pp_unop unop (pp_expr ~level:(level + 1)) expr
  | BinE { binop; expr_l; expr_r } ->
      F.fprintf fmt "((%a) %a (%a))"
        (pp_expr ~level:(level + 1))
        expr_l pp_binop binop
        (pp_expr ~level:(level + 1))
        expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      F.fprintf fmt "((%a) ? (%a) : (%a))"
        (pp_expr ~level:(level + 1))
        expr_cond
        (pp_expr ~level:(level + 1))
        expr_then
        (pp_expr ~level:(level + 1))
        expr_else
  | CastE { typ; expr } -> (
      match typ.it with
      | SetT _ ->
          F.fprintf fmt "/* %a */ %a"
            (pp_typ ~level:(level + 1))
            typ
            (pp_expr ~level:(level + 1))
            expr
      | _ ->
          F.fprintf fmt "((%a) (%a))"
            (pp_typ ~level:(level + 1))
            typ
            (pp_expr ~level:(level + 1))
            expr)
  | MaskE { expr_base; expr_mask } ->
      F.fprintf fmt "%a &&& %a"
        (pp_expr ~level:(level + 1))
        expr_base
        (pp_expr ~level:(level + 1))
        expr_mask
  | RangeE { expr_lb; expr_ub } ->
      F.fprintf fmt "%a .. %a"
        (pp_expr ~level:(level + 1))
        expr_lb
        (pp_expr ~level:(level + 1))
        expr_ub
  | SelectE { exprs_select; cases } ->
      F.fprintf fmt "select(%a) {\n%a\n%s}"
        (pp_exprs ~level:(level + 1))
        exprs_select
        (pp_select_cases ~level:(level + 1))
        cases (indent level)
  | ArrAccE { expr_base; expr_idx } ->
      F.fprintf fmt "%a[%a]"
        (pp_expr ~level:(level + 1))
        expr_base
        (pp_expr ~level:(level + 1))
        expr_idx
  | BitAccE { expr_base; value_lo; value_hi } ->
      F.fprintf fmt "%a[%a:%a]"
        (pp_expr ~level:(level + 1))
        expr_base
        (pp_value ~level:(level + 1))
        value_hi
        (pp_value ~level:(level + 1))
        value_lo
  | ErrAccE { member } -> F.fprintf fmt "error.%a" pp_member member
  | TypeAccE { var_base; member } ->
      F.fprintf fmt "%a.%a" pp_var var_base pp_member member
  | ExprAccE { expr_base; member } ->
      F.fprintf fmt "%a.%a"
        (pp_expr ~level:(level + 1))
        expr_base pp_member member
  | CallFuncE { var_func; targs; args } ->
      F.fprintf fmt "%a%a%a" pp_var var_func
        (pp_targs ~level:(level + 1))
        targs pp_args args
  | CallMethodE { expr_base; member; targs; args } ->
      F.fprintf fmt "%a.%a%a%a"
        (pp_expr ~level:(level + 1))
        expr_base pp_member member
        (pp_targs ~level:(level + 1))
        targs pp_args args
  | CallTypeE { typ; member } ->
      F.fprintf fmt "%a.%a()" (pp_typ ~level:(level + 1)) typ pp_member member
  | InstE { var_inst; targs; targs_hidden; args } ->
      F.fprintf fmt "%a%a%a%s" pp_var var_inst
        (pp_targs ~level:(level + 1))
        targs pp_args args
        (if List.length targs_hidden = 0 then ""
         else
           F.asprintf " /* %n wildcards matched */" (List.length targs_hidden))

and pp_expr ?(level = 0) fmt expr = pp_expr' ~level fmt expr.it

and pp_exprs ?(level = 0) fmt exprs =
  pp_list (pp_expr ~level) ~sep:Comma fmt exprs

(* Keyset expressions *)

and pp_keysets fmt keysets = P.pp_keysets pp_expr fmt keysets

(* Select-cases for select *)

and pp_select_cases ?(level = 0) fmt select_cases =
  P.pp_select_cases ~level pp_expr fmt select_cases

(* Statements *)

and pp_stmt' ?(level = 0) fmt stmt' =
  match stmt' with
  | EmptyS -> F.fprintf fmt ";"
  | AssignS { expr_l; expr_r } ->
      F.fprintf fmt "%a = %a;"
        (pp_expr ~level:(level + 1))
        expr_l
        (pp_expr ~level:(level + 1))
        expr_r
  | SwitchS { expr_switch; cases } ->
      F.fprintf fmt "switch (%a) {\n%a\n%s}"
        (pp_expr ~level:(level + 1))
        expr_switch
        (pp_switch_cases ~level:(level + 1))
        cases (indent level)
  | IfS { expr_cond; stmt_then; stmt_else } -> (
      match stmt_else.it with
      | EmptyS ->
          F.fprintf fmt "if (%a) %a"
            (pp_expr ~level:(level + 1))
            expr_cond (pp_stmt ~level) stmt_then
      | _ ->
          F.fprintf fmt "if (%a) %a\n%selse %a"
            (pp_expr ~level:(level + 1))
            expr_cond (pp_stmt ~level) stmt_then (indent level) (pp_stmt ~level)
            stmt_else)
  | BlockS { block } -> pp_block ~level fmt block
  | ExitS -> F.fprintf fmt "exit;"
  | RetS { expr_ret } -> (
      match expr_ret with
      | Some expr_ret ->
          F.fprintf fmt "return %a;" (pp_expr ~level:(level + 1)) expr_ret
      | None -> F.fprintf fmt "return;")
  | CallFuncS { var_func; targs; args } ->
      F.fprintf fmt "%a%a%a;" pp_var var_func
        (pp_targs ~level:(level + 1))
        targs pp_args args
  | CallMethodS { expr_base; member; targs; args } ->
      F.fprintf fmt "%a.%a%a%a;"
        (pp_expr ~level:(level + 1))
        expr_base pp_member member
        (pp_targs ~level:(level + 1))
        targs pp_args args
  | CallInstS { typ = _typ; var_inst; targs; args } ->
      F.fprintf fmt "%a%a.apply%a;" pp_var var_inst
        (pp_targs ~level:(level + 1))
        targs pp_args args
  | TransS { expr_label } ->
      let sexpr_label =
        F.asprintf "%a" (pp_expr ~level:(level + 1)) expr_label
      in
      let trailing_semicolon =
        if String.starts_with ~prefix:"select(" sexpr_label then "" else ";"
      in
      F.fprintf fmt "transition %s%s" sexpr_label trailing_semicolon
  | DeclS { decl } -> pp_decl ~level fmt decl

and pp_stmt ?(level = 0) fmt stmt = pp_stmt' ~level fmt stmt.it

(* Blocks (sequence of statements) *)

and pp_block ?(level = 0) fmt block =
  P.pp_block ~level pp_expr pp_stmt fmt block

(* Match-cases for switch *)

and pp_switch_cases ?(level = 0) fmt switch_cases =
  P.pp_switch_cases ~level pp_expr pp_stmt fmt switch_cases

(* Declarations *)

and pp_decl' ?(level = 0) fmt decl' =
  match decl' with
  | ConstD { id; typ; value; annos = _annos } ->
      F.fprintf fmt "const %a %a = %a;"
        (pp_typ ~level:(level + 1))
        typ pp_id id
        (pp_value ~level:(level + 1))
        value
  | VarD { id; typ; init; annos = _annos } -> (
      match init with
      | Some expr_init ->
          F.fprintf fmt "%a %a = %a;"
            (pp_typ ~level:(level + 1))
            typ pp_id id (pp_expr ~level:0) expr_init
      | None -> F.fprintf fmt "%a %a;" (pp_typ ~level:(level + 1)) typ pp_id id)
  | ErrD { members } ->
      F.fprintf fmt "error {\n%a\n%s}"
        (pp_members ~level:(level + 1))
        members (indent level)
  | MatchKindD { members } ->
      F.fprintf fmt "match_kind {\n%a\n%s}"
        (pp_members ~level:(level + 1))
        members (indent level)
  | InstD
      {
        id;
        typ = _typ;
        var_inst;
        targs;
        targs_hidden;
        args;
        init;
        annos = _annos;
      } -> (
      match init with
      | [] ->
          F.fprintf fmt "%a%a%a %a;%s" pp_var var_inst
            (pp_targs ~level:(level + 1))
            targs pp_args args pp_id id
            (if List.length targs_hidden = 0 then ""
             else
               F.asprintf " /* %n wildcards matched */"
                 (List.length targs_hidden))
      | init ->
          F.fprintf fmt "%a%a%a %a%s = {\n%a\n%s};" pp_var var_inst
            (pp_targs ~level:(level + 1))
            targs pp_args args pp_id id
            (if List.length targs_hidden = 0 then ""
             else
               F.asprintf " /* %n wildcards matched */"
                 (List.length targs_hidden))
            (pp_decls ~level:(level + 1))
            init (indent level))
  | StructD { id; tparams; tparams_hidden; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (typ, member)) fields in
      F.fprintf fmt "struct %a%a {\n%a\n%s}" pp_id id pp_tparams
        (tparams @ tparams_hidden)
        (pp_pairs ~trailing:true ~level:(level + 1) pp_typ pp_member ~rel:Space
           ~sep:SemicolonNl)
        fields (indent level)
  | HeaderD { id; tparams; tparams_hidden; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (typ, member)) fields in
      F.fprintf fmt "header %a%a {\n%a\n%s}" pp_id id pp_tparams
        (tparams @ tparams_hidden)
        (pp_pairs ~trailing:true ~level:(level + 1) pp_typ pp_member ~rel:Space
           ~sep:SemicolonNl)
        fields (indent level)
  | UnionD { id; tparams; tparams_hidden; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (typ, member)) fields in
      F.fprintf fmt "header_union %a%a {\n%a\n%s}" pp_id id pp_tparams
        (tparams @ tparams_hidden)
        (pp_pairs ~trailing:true ~level:(level + 1) pp_typ pp_member ~rel:Space
           ~sep:SemicolonNl)
        fields (indent level)
  | EnumD { id; members; annos = _annos } ->
      F.fprintf fmt "enum %a {\n%a\n%s};" pp_id id
        (pp_members ~level:(level + 1))
        members (indent level)
  | SEnumD { id; typ; fields; annos = _annos } ->
      F.fprintf fmt "enum %a %a {\n%a\n%s};"
        (pp_typ ~level:(level + 1))
        typ pp_id id
        (pp_pairs pp_id (pp_value ~level:(level + 1)) ~rel:Eq ~sep:CommaNl)
        fields (indent level)
  | NewTypeD { id; typdef; annos = _annos } -> (
      match typdef with
      | Left typ ->
          F.fprintf fmt "type %a %a;" (pp_typ ~level:(level + 1)) typ pp_id id
      | Right decl ->
          F.fprintf fmt "type %a %a;" (pp_decl ~level:(level + 1)) decl pp_id id
      )
  | TypeDefD { id; typdef; annos = _annos } -> (
      match typdef with
      | Left typ ->
          F.fprintf fmt "typedef %a %a;"
            (pp_typ ~level:(level + 1))
            typ pp_id id
      | Right decl ->
          F.fprintf fmt "typedef %a %a;"
            (pp_decl ~level:(level + 1))
            decl pp_id id)
  | ValueSetD { id; typ; size; annos = _annos } ->
      F.fprintf fmt "value_set<%a>(%a) %a;"
        (pp_typ ~level:(level + 1))
        typ
        (pp_expr ~level:(level + 1))
        size pp_id id
  | ParserTypeD { id; tparams; tparams_hidden; params; annos = _annos } ->
      F.fprintf fmt "parser %a%a%a;" pp_id id pp_tparams
        (tparams @ tparams_hidden)
        (pp_params ~level:(level + 1))
        params
  | ParserD { id; tparams; params; cparams; locals; states; annos = _annos } ->
      F.fprintf fmt "parser %a%a%a%a {\n%a\n%a\n%s}" pp_id id pp_tparams tparams
        (pp_params ~level:(level + 1))
        params
        (pp_cparams ~level:(level + 1))
        cparams
        (pp_decls ~level:(level + 1))
        locals
        (pp_parser_states ~level:(level + 1))
        states (indent level)
  | TableD { id; typ = _typ; table; annos = _annos } ->
      F.fprintf fmt "table %a %a" pp_id id (pp_table ~level) table
  | ControlTypeD { id; tparams; tparams_hidden; params; annos = _annos } ->
      F.fprintf fmt "control %a%a%a;" pp_id id pp_tparams
        (tparams @ tparams_hidden)
        (pp_params ~level:(level + 1))
        params
  | ControlD { id; tparams; params; cparams; locals; body; annos = _annos } ->
      F.fprintf fmt "control %a%a%a%a {\n%a\n%sapply %a\n%s}" pp_id id
        pp_tparams tparams
        (pp_params ~level:(level + 1))
        params
        (pp_cparams ~level:(level + 1))
        cparams
        (pp_decls ~level:(level + 1))
        locals
        (indent (level + 1))
        (pp_block ~level:(level + 1))
        body (indent level)
  | ActionD { id; params; body; annos = _annos } ->
      F.fprintf fmt "action %a%a %a" pp_id id
        (pp_params ~level:(level + 1))
        params (pp_block ~level) body
  | FuncD { id; typ_ret; tparams; tparams_hidden; params; body } ->
      F.fprintf fmt "%a %a%a%a %a"
        (pp_typ ~level:(level + 1))
        typ_ret pp_id id pp_tparams (tparams @ tparams_hidden)
        (pp_params ~level:(level + 1))
        params (pp_block ~level) body
  | ExternFuncD { id; typ_ret; tparams; tparams_hidden; params; annos = _annos }
    ->
      F.fprintf fmt "extern %a %a%a%a;"
        (pp_typ ~level:(level + 1))
        typ_ret pp_id id pp_tparams (tparams @ tparams_hidden)
        (pp_params ~level:(level + 1))
        params
  | ExternObjectD { id; tparams; mthds; annos = _annos } ->
      F.fprintf fmt "extern %a%a {\n%a\n%s}" pp_id id pp_tparams tparams
        (pp_mthds ~level:(level + 1))
        mthds (indent level)
  | PackageTypeD { id; tparams; tparams_hidden; cparams; annos = _annos } ->
      assert (List.length tparams_hidden = 0);
      F.fprintf fmt "package %a%a%a;" pp_id id pp_tparams tparams
        (pp_cparams ~level:(level + 1))
        cparams

and pp_decl ?(level = 0) fmt decl = pp_decl' ~level fmt decl.it

and pp_decls ?(level = 0) fmt decls =
  pp_list ~level (pp_decl ~level) ~sep:Nl fmt decls

(* Parser states *)

and pp_parser_states ?(level = 0) fmt parser_states =
  P.pp_parser_states ~level pp_expr pp_stmt fmt parser_states

(* Tables *)

and pp_table ?(level = 0) fmt table =
  P.pp_table ~level pp_expr pp_table_action pp_table_entry fmt table

(* Table action references *)

and pp_table_action' ?(level = 0) fmt table_action' =
  let var, args, _annos, params_data, params_control = table_action' in
  let comment_data_control params_data params_control =
    if List.length params_data = 0 && List.length params_control = 0 then ""
    else if List.length params_data = 0 then
      F.asprintf " /* control: %a */"
        (pp_params ~level:(level + 1))
        params_control
    else if List.length params_control = 0 then
      F.asprintf " /* data: %a */" (pp_params ~level:(level + 1)) params_data
    else
      F.asprintf " /* data: %a, control: %a */"
        (pp_params ~level:(level + 1))
        params_data
        (pp_params ~level:(level + 1))
        params_control
  in
  match args with
  | [] ->
      F.fprintf fmt "%a;%s" pp_var var
        (comment_data_control params_data params_control)
  | _ ->
      F.fprintf fmt "%a%a;%s" pp_var var pp_args args
        (comment_data_control params_data params_control)

and pp_table_action ?(level = 0) fmt table_action =
  pp_table_action' ~level fmt table_action.it

(* Table entries *)

and pp_table_entry' ?(level = 0) ?(table_entries_const = false) fmt table_entry'
    =
  let table_entry_const, keysets, table_action, table_entry_priority, _annos =
    table_entry'
  in
  F.fprintf fmt "%s%s%s%a%s%s%a : %a"
    (if table_entry_const then "const " else "")
    (if table_entries_const then "/* " else "")
    (if table_entry_priority |> Option.is_some then "priority = " else "")
    (pp_option pp_value) table_entry_priority
    (if table_entry_priority |> Option.is_some then " : " else "")
    (if table_entries_const then "*/ " else "")
    pp_keysets keysets (pp_table_action ~level) table_action

and pp_table_entry ?(level = 0) ?(table_entries_const = false) fmt table_entry =
  pp_table_entry' ~level ~table_entries_const fmt table_entry.it

(* Methods *)

and pp_mthd' ?(level = 0) fmt mthd' =
  match mthd' with
  | ExternConsM { id; tparams_hidden; cparams; annos = _annos } ->
      assert (List.length tparams_hidden = 0);
      F.fprintf fmt "%a%a;" pp_id id (pp_cparams ~level:(level + 1)) cparams
  | ExternAbstractM
      { id; typ_ret; tparams; tparams_hidden; params; annos = _annos } ->
      F.fprintf fmt "abstract %a %a%a%a;"
        (pp_typ ~level:(level + 1))
        typ_ret pp_id id pp_tparams (tparams @ tparams_hidden)
        (pp_params ~level:(level + 1))
        params
  | ExternM { id; typ_ret; tparams; tparams_hidden; params; annos = _annos } ->
      F.fprintf fmt "%a %a%a%a;"
        (pp_typ ~level:(level + 1))
        typ_ret pp_id id pp_tparams (tparams @ tparams_hidden)
        (pp_params ~level:(level + 1))
        params

and pp_mthd ?(level = 0) fmt mthd = pp_mthd' ~level fmt mthd.it
and pp_mthds ?(level = 0) fmt mthds = pp_list ~level pp_mthd ~sep:Nl fmt mthds

(* Program *)

let pp_program fmt program = P.pp_program pp_decl fmt program
