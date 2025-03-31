open Xl.Atom
module P4 = P4el.Ast
open Il.Ast
open Util.Source

(* Helpers *)

let in_opt (do_in : 'a -> value) (opt : 'a option) : value =
  let vopt = Option.map do_in opt in
  OptV vopt $$$ Ctx.note_source ()

let in_list (do_in : 'a -> value) (lst : 'a list) : value =
  let vlst = List.map do_in lst in
  ListV vlst $$$ Ctx.note_source ()

let in_pair (do_in_a : 'a -> value) (do_in_b : 'b -> value) ((a, b) : 'a * 'b) :
    value =
  TupleV [ do_in_a a; do_in_b b ] $$$ Ctx.note_source ()

let atom (s : string) : atom = Atom s $ no_region

(* Booleans *)

let in_bool (boolean : bool) : value = BoolV boolean $$$ Ctx.note_source ()

(* Numbers *)

let in_num (num : P4.num) : value =
  match num.it with
  | i, Some (width, signed) ->
      let mixop =
        if signed then [ [ atom "FINT" ]; []; [] ]
        else [ [ atom "FBIT" ]; []; [] ]
      in
      let vwidth = NumV (`Nat width) $$$ Ctx.note_source () in
      let vint = NumV (`Int i) $$$ Ctx.note_source () in
      CaseV (mixop, [ vwidth; vint ]) $$$ Ctx.note_source ()
  | i, None ->
      let mixop = [ [ atom "INT" ]; [] ] in
      let vint = NumV (`Int i) $$$ Ctx.note_source () in
      CaseV (mixop, [ vint ]) $$$ Ctx.note_source ()

(* Texts *)

let in_text (text : P4.text) : value = TextV text.it $$$ Ctx.note_source ()

(* Identifiers *)

let in_id (id : P4.id) : value = TextV id.it $$$ Ctx.note_source ()

(* Variables (scoped identifiers) *)

let in_var (var : P4.var) : value =
  match var.it with
  | Top id ->
      let mixop = [ [ atom "TOP" ]; [] ] in
      let vid = in_id id in
      CaseV (mixop, [ vid ]) $$$ Ctx.note_source ()
  | Current id ->
      let mixop = [ [ atom "CURRENT" ]; [] ] in
      let vid = in_id id in
      CaseV (mixop, [ vid ]) $$$ Ctx.note_source ()

(* Members *)

let rec in_member (member : P4.member) : value =
  TextV member.it $$$ Ctx.note_source ()

and in_members (members : P4.member list) : value = in_list in_member members

(* Match kinds *)

let in_match_kind (match_kind : P4.match_kind) : value =
  TextV match_kind.it $$$ Ctx.note_source ()

(* State labels *)

let in_state_label (state_label : P4.state_label) : value =
  TextV state_label.it $$$ Ctx.note_source ()

(* Unary operators *)

let in_unop (unop : P4.unop) : value =
  match unop.it with
  | BNotOp ->
      let mixop = [ [ atom "BNOT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | LNotOp ->
      let mixop = [ [ atom "LNOT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | UPlusOp ->
      let mixop = [ [ atom "UPLUS" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | UMinusOp ->
      let mixop = [ [ atom "UMINUS" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()

(* Binary operators *)

let in_binop (binop : P4.binop) : value =
  match binop.it with
  | PlusOp ->
      let mixop = [ [ atom "PLUS" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | SPlusOp ->
      let mixop = [ [ atom "SPLUS" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | MinusOp ->
      let mixop = [ [ atom "MINUS" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | SMinusOp ->
      let mixop = [ [ atom "SMINUS" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | MulOp ->
      let mixop = [ [ atom "MUL" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | DivOp ->
      let mixop = [ [ atom "DIV" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | ModOp ->
      let mixop = [ [ atom "MOD" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | ShlOp ->
      let mixop = [ [ atom "SHL" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | ShrOp ->
      let mixop = [ [ atom "SHR" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | LeOp ->
      let mixop = [ [ atom "LE" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | GeOp ->
      let mixop = [ [ atom "GE" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | LtOp ->
      let mixop = [ [ atom "LT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | GtOp ->
      let mixop = [ [ atom "GT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | EqOp ->
      let mixop = [ [ atom "EQ" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | NeOp ->
      let mixop = [ [ atom "NE" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | BAndOp ->
      let mixop = [ [ atom "BAND" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | BXorOp ->
      let mixop = [ [ atom "BXOR" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | BOrOp ->
      let mixop = [ [ atom "BOR" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | ConcatOp ->
      let mixop = [ [ atom "CONCAT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | LAndOp ->
      let mixop = [ [ atom "LAND" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | LOrOp ->
      let mixop = [ [ atom "LOR" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()

(* Directions *)

let in_dir (dir : P4.dir) : value =
  match dir.it with
  | No ->
      let mixop = [ [ atom "NO" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | In ->
      let mixop = [ [ atom "IN" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | Out ->
      let mixop = [ [ atom "OUT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | InOut ->
      let mixop = [ [ atom "INOUT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()

(* Types *)

let rec in_typ (typ : P4.typ) : value =
  match typ.it with
  | VoidT ->
      let mixop = [ [ atom "VoidT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | ErrT ->
      let mixop = [ [ atom "ErrT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | MatchKindT ->
      let mixop = [ [ atom "MatchKindT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | StrT ->
      let mixop = [ [ atom "StrT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | BoolT ->
      let mixop = [ [ atom "BoolT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | IntT ->
      let mixop = [ [ atom "IntT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | FIntT expr ->
      let mixop = [ [ atom "FIntT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ]) $$$ Ctx.note_source ()
  | FBitT expr ->
      let mixop = [ [ atom "FBitT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ]) $$$ Ctx.note_source ()
  | VBitT expr ->
      let mixop = [ [ atom "VBitT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ]) $$$ Ctx.note_source ()
  | StackT (typ, expr) ->
      let mixop = [ [ atom "StackT" ]; []; [] ] in
      let vtyp = in_typ typ in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vtyp; vexpr ]) $$$ Ctx.note_source ()
  | ListT typ ->
      let mixop = [ [ atom "ListT" ]; [] ] in
      let vtyp = in_typ typ in
      CaseV (mixop, [ vtyp ]) $$$ Ctx.note_source ()
  | TupleT typs ->
      let mixop = [ [ atom "TupleT" ]; [] ] in
      let vtyps = in_typs typs in
      CaseV (mixop, [ vtyps ]) $$$ Ctx.note_source ()
  | NameT var ->
      let mixop = [ [ atom "NameT" ]; [] ] in
      let vvar = in_var var in
      CaseV (mixop, [ vvar ]) $$$ Ctx.note_source ()
  | SpecT (var, targs) ->
      let mixop = [ [ atom "SpecT" ]; []; [] ] in
      let vvar = in_var var in
      let vtargs = in_targs targs in
      CaseV (mixop, [ vvar; vtargs ]) $$$ Ctx.note_source ()
  | AnyT ->
      let mixop = [ [ atom "AnyT" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()

and in_typs (typs : P4.typ list) : value = in_list in_typ typs

(* Type parameters *)

and in_tparam (tparam : P4.tparam) : value =
  TextV tparam.it $$$ Ctx.note_source ()

and in_tparams (tparams : P4.tparam list) : value = in_list in_tparam tparams

(* Parameters *)

and in_param (param : P4.param) : value =
  let id, dir, typ, expr_opt, _ = param.it in
  let mixop = [ []; []; []; []; [] ] in
  let vid = in_id id in
  let vdir = in_dir dir in
  let vtyp = in_typ typ in
  let vexpr_opt = in_opt in_expr expr_opt in
  CaseV (mixop, [ vid; vdir; vtyp; vexpr_opt ]) $$$ Ctx.note_source ()

and in_params (params : P4.param list) : value = in_list in_param params

(* Constructor parameters *)

and in_cparam (cparam : P4.cparam) : value = in_param cparam
and in_cparams (cparams : P4.cparam list) : value = in_list in_cparam cparams

(* Type arguments *)

and in_targ (targ : P4.targ) : value = in_typ targ
and in_targs (targs : P4.targ list) : value = in_list in_targ targs

(* Arguments *)

and in_arg (arg : P4.arg) : value =
  match arg.it with
  | ExprA expr ->
      let mixop = [ [ atom "ExprA" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ]) $$$ Ctx.note_source ()
  | NameA (id, expr_opt) ->
      let mixop = [ [ atom "NameA" ]; []; [] ] in
      let vid = in_id id in
      let vexpr_opt = in_opt in_expr expr_opt in
      CaseV (mixop, [ vid; vexpr_opt ]) $$$ Ctx.note_source ()
  | AnyA ->
      let mixop = [ [ atom "AnyA" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()

and in_args (args : P4.arg list) : value = in_list in_arg args

(* Expressions *)

and in_expr (expr : P4.expr) : value =
  match expr.it with
  | BoolE { boolean } ->
      let mixop = [ [ atom "BoolE" ]; [] ] in
      let vboolean = in_bool boolean in
      CaseV (mixop, [ vboolean ]) $$$ Ctx.note_source ()
  | StrE { text } ->
      let mixop = [ [ atom "StrE" ]; [] ] in
      let vtext = in_text text in
      CaseV (mixop, [ vtext ]) $$$ Ctx.note_source ()
  | NumE { num } ->
      let mixop = [ [ atom "NumE" ]; [] ] in
      let vnum = in_num num in
      CaseV (mixop, [ vnum ]) $$$ Ctx.note_source ()
  | VarE { var } ->
      let mixop = [ [ atom "NameE" ]; [] ] in
      let vvar = in_var var in
      CaseV (mixop, [ vvar ]) $$$ Ctx.note_source ()
  | SeqE { exprs } ->
      let mixop = [ [ atom "SeqE" ]; [] ] in
      let vexprs = in_exprs exprs in
      CaseV (mixop, [ vexprs ]) $$$ Ctx.note_source ()
  | SeqDefaultE { exprs } ->
      let mixop = [ [ atom "SeqDefaultE" ]; [] ] in
      let vexprs = in_exprs exprs in
      CaseV (mixop, [ vexprs ]) $$$ Ctx.note_source ()
  | RecordE { fields } ->
      let mixop = [ [ atom "RecordE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vfields ]) $$$ Ctx.note_source ()
  | RecordDefaultE { fields } ->
      let mixop = [ [ atom "RecordDefaultE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vfields ]) $$$ Ctx.note_source ()
  | DefaultE ->
      let mixop = [ [ atom "DefaultE" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | InvalidE ->
      let mixop = [ [ atom "InvalidE" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | UnE { unop; expr } ->
      let mixop = [ [ atom "UnE" ]; []; [] ] in
      let vunop = in_unop unop in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vunop; vexpr ]) $$$ Ctx.note_source ()
  | BinE { binop; expr_l; expr_r } ->
      let mixop = [ [ atom "BinE" ]; []; []; [] ] in
      let vbinop = in_binop binop in
      let vexpr_l = in_expr expr_l in
      let vexpr_r = in_expr expr_r in
      CaseV (mixop, [ vbinop; vexpr_l; vexpr_r ]) $$$ Ctx.note_source ()
  | TernE { expr_cond; expr_then; expr_else } ->
      let mixop = [ [ atom "TernE" ]; []; []; [] ] in
      let vexpr_cond = in_expr expr_cond in
      let vexpr_then = in_expr expr_then in
      let vexpr_else = in_expr expr_else in
      CaseV (mixop, [ vexpr_cond; vexpr_then; vexpr_else ])
      $$$ Ctx.note_source ()
  | CastE { typ; expr } ->
      let mixop = [ [ atom "CastE" ]; []; [] ] in
      let vtyp = in_typ typ in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vtyp; vexpr ]) $$$ Ctx.note_source ()
  | MaskE { expr_base; expr_mask } ->
      let mixop = [ [ atom "MaskE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_mask = in_expr expr_mask in
      CaseV (mixop, [ vexpr_base; vexpr_mask ]) $$$ Ctx.note_source ()
  | RangeE { expr_lb; expr_ub } ->
      let mixop = [ [ atom "RangeE" ]; []; [] ] in
      let vexpr_lb = in_expr expr_lb in
      let vexpr_ub = in_expr expr_ub in
      CaseV (mixop, [ vexpr_lb; vexpr_ub ]) $$$ Ctx.note_source ()
  | SelectE { exprs_select; cases } ->
      let mixop = [ [ atom "SelectE" ]; []; [] ] in
      let vexprs_select = in_exprs exprs_select in
      let vcases = in_select_cases cases in
      CaseV (mixop, [ vexprs_select; vcases ]) $$$ Ctx.note_source ()
  | ArrAccE { expr_base; expr_idx } ->
      let mixop = [ [ atom "ArrAccE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_idx = in_expr expr_idx in
      CaseV (mixop, [ vexpr_base; vexpr_idx ]) $$$ Ctx.note_source ()
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      let mixop = [ [ atom "BitAccE" ]; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_lo = in_expr expr_lo in
      let vexpr_hi = in_expr expr_hi in
      CaseV (mixop, [ vexpr_base; vexpr_lo; vexpr_hi ]) $$$ Ctx.note_source ()
  | ErrAccE { member } ->
      let mixop = [ [ atom "ErrAccE" ]; [] ] in
      let vmember = in_member member in
      CaseV (mixop, [ vmember ]) $$$ Ctx.note_source ()
  | TypeAccE { var_base; member } ->
      let mixop = [ [ atom "TypeAccE" ]; []; [] ] in
      let vvar_base = in_var var_base in
      let vmember = in_member member in
      CaseV (mixop, [ vvar_base; vmember ]) $$$ Ctx.note_source ()
  | ExprAccE { expr_base; member } ->
      let mixop = [ [ atom "ExprAccE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      CaseV (mixop, [ vexpr_base; vmember ]) $$$ Ctx.note_source ()
  | CallFuncE { var_func; targs; args } ->
      let mixop = [ [ atom "CallFuncE" ]; []; []; [] ] in
      let vvar_func = in_var var_func in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_func; vtargs; vargs ]) $$$ Ctx.note_source ()
  | CallMethodE { expr_base; member; targs; args } ->
      let mixop = [ [ atom "CallMethodE" ]; []; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
      $$$ Ctx.note_source ()
  | CallTypeE { var_typ; member; targs; args } ->
      let mixop = [ [ atom "CallTypeE" ]; []; []; []; [] ] in
      let vvar_typ = in_var var_typ in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_typ; vmember; vtargs; vargs ]) $$$ Ctx.note_source ()
  | InstE { var_inst; targs; args } ->
      let mixop = [ [ atom "InstE" ]; []; []; [] ] in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_inst; vtargs; vargs ]) $$$ Ctx.note_source ()

and in_exprs (exprs : P4.expr list) : value = in_list in_expr exprs

(* Keyset expressions *)

and in_keyset (keyset : P4.keyset) : value =
  match keyset.it with
  | ExprK expr ->
      let mixop = [ [ atom "ExprK" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ]) $$$ Ctx.note_source ()
  | DefaultK ->
      let mixop = [ [ atom "DefaultK" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | AnyK ->
      let mixop = [ [ atom "AnyK" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()

and in_keysets (keysets : P4.keyset list) : value = in_list in_keyset keysets

(* Select-cases for select *)

and in_select_case (select_case : P4.select_case) : value =
  let keysets, state_label = select_case.it in
  let mixop = [ []; []; [] ] in
  let vkeysets = in_keysets keysets in
  let vstate_label = in_state_label state_label in
  CaseV (mixop, [ vkeysets; vstate_label ]) $$$ Ctx.note_source ()

and in_select_cases (select_cases : P4.select_case list) : value =
  in_list in_select_case select_cases

(* Statements *)

and in_stmt (stmt : P4.stmt) : value =
  match stmt.it with
  | EmptyS ->
      let mixop = [ [ atom "EmptyS" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | AssignS { expr_l; expr_r } ->
      let mixop = [ [ atom "AssignS" ]; []; [] ] in
      let vexpr_l = in_expr expr_l in
      let vexpr_r = in_expr expr_r in
      CaseV (mixop, [ vexpr_l; vexpr_r ]) $$$ Ctx.note_source ()
  | SwitchS { expr_switch; cases } ->
      let mixop = [ [ atom "SwitchS" ]; []; [] ] in
      let vexpr_switch = in_expr expr_switch in
      let vcases = in_switch_cases cases in
      CaseV (mixop, [ vexpr_switch; vcases ]) $$$ Ctx.note_source ()
  | IfS { expr_cond; stmt_then; stmt_else } ->
      let mixop = [ [ atom "IfS" ]; []; []; [] ] in
      let vexpr_cond = in_expr expr_cond in
      let vstmt_then = in_stmt stmt_then in
      let vstmt_else = in_stmt stmt_else in
      CaseV (mixop, [ vexpr_cond; vstmt_then; vstmt_else ])
      $$$ Ctx.note_source ()
  | BlockS { block } ->
      let mixop = [ [ atom "BlockS" ]; [] ] in
      let vblock = in_block block in
      CaseV (mixop, [ vblock ]) $$$ Ctx.note_source ()
  | ExitS ->
      let mixop = [ [ atom "ExitS" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()
  | RetS { expr_ret } ->
      let mixop = [ [ atom "RetS" ]; [] ] in
      let vexpr_ret = in_opt in_expr expr_ret in
      CaseV (mixop, [ vexpr_ret ]) $$$ Ctx.note_source ()
  | CallFuncS { var_func; targs; args } ->
      let mixop = [ [ atom "CallFuncS" ]; []; []; [] ] in
      let vvar_func = in_var var_func in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_func; vtargs; vargs ]) $$$ Ctx.note_source ()
  | CallMethodS { expr_base; member; targs; args } ->
      let mixop = [ [ atom "CallMethodS" ]; []; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
      $$$ Ctx.note_source ()
  | CallInstS { var_inst; targs; args } ->
      let mixop = [ [ atom "CallInstS" ]; []; []; [] ] in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_inst; vtargs; vargs ]) $$$ Ctx.note_source ()
  | TransS { expr_label } ->
      let mixop = [ [ atom "TransS" ]; [] ] in
      let vexpr_label = in_expr expr_label in
      CaseV (mixop, [ vexpr_label ]) $$$ Ctx.note_source ()
  | DeclS { decl } ->
      let mixop = [ [ atom "DeclS" ]; [] ] in
      let vdecl = in_decl decl in
      CaseV (mixop, [ vdecl ]) $$$ Ctx.note_source ()

and in_stmts (stmts : P4.stmt list) : value = in_list in_stmt stmts

(* Blocks (sequence of statements) *)

and in_block (block : P4.block) : value =
  let stmts, _ = block.it in
  let mixop = [ [ atom "BlockB" ]; [] ] in
  let vstmts = in_stmts stmts in
  CaseV (mixop, [ vstmts ]) $$$ Ctx.note_source ()

(* Match-cases for switch *)

and in_switch_label (switch_label : P4.switch_label) : value =
  match switch_label.it with
  | ExprL expr ->
      let mixop = [ [ atom "ExprL" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ]) $$$ Ctx.note_source ()
  | DefaultL ->
      let mixop = [ [ atom "DefaultL" ] ] in
      CaseV (mixop, []) $$$ Ctx.note_source ()

and in_switch_case (switch_case : P4.switch_case) : value =
  match switch_case.it with
  | MatchC (switch_label, block) ->
      let mixop = [ [ atom "MatchC" ]; []; [] ] in
      let vswitch_label = in_switch_label switch_label in
      let vblock = in_block block in
      CaseV (mixop, [ vswitch_label; vblock ]) $$$ Ctx.note_source ()
  | FallC switch_label ->
      let mixop = [ [ atom "FallC" ]; [] ] in
      let vswitch_label = in_switch_label switch_label in
      CaseV (mixop, [ vswitch_label ]) $$$ Ctx.note_source ()

and in_switch_cases (switch_cases : P4.switch_case list) : value =
  in_list in_switch_case switch_cases

(* Declarations *)

and in_typdef (typdef : (P4.typ, P4.decl) P4.alt) : value =
  match typdef with
  | Left typ ->
      let mixop = [ [ atom "TypeD" ]; [] ] in
      let vtyp = in_typ typ in
      CaseV (mixop, [ vtyp ]) $$$ Ctx.note_source ()
  | Right decl ->
      let mixop = [ [ atom "DeclD" ]; [] ] in
      let vdecl = in_decl decl in
      CaseV (mixop, [ vdecl ]) $$$ Ctx.note_source ()

and in_decl (decl : P4.decl) : value =
  match decl.it with
  | ConstD { id; typ; value; _ } ->
      let mixop = [ [ atom "ConstD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vvalue = in_expr value in
      CaseV (mixop, [ vid; vtyp; vvalue ]) $$$ Ctx.note_source ()
  | VarD { id; typ; init; _ } ->
      let mixop = [ [ atom "VarD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vinit = in_opt in_expr init in
      CaseV (mixop, [ vid; vtyp; vinit ]) $$$ Ctx.note_source ()
  | ErrD { members } ->
      let mixop = [ [ atom "ErrD" ]; [] ] in
      let vmembers = in_members members in
      CaseV (mixop, [ vmembers ]) $$$ Ctx.note_source ()
  | MatchKindD { members } ->
      let mixop = [ [ atom "MatchKindD" ]; [] ] in
      let vmembers = in_members members in
      CaseV (mixop, [ vmembers ]) $$$ Ctx.note_source ()
  | InstD { id; var_inst; targs; args; init; _ } ->
      let mixop = [ [ atom "InstD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      let vinit = in_decls init in
      CaseV (mixop, [ vid; vvar_inst; vtargs; vargs; vinit ])
      $$$ Ctx.note_source ()
  | StructD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "StructD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Ctx.note_source ()
  | HeaderD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "HeaderD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Ctx.note_source ()
  | UnionD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "UnionD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Ctx.note_source ()
  | EnumD { id; members; _ } ->
      let mixop = [ [ atom "EnumD" ]; []; [] ] in
      let vid = in_id id in
      let vmembers = in_members members in
      CaseV (mixop, [ vid; vmembers ]) $$$ Ctx.note_source ()
  | SEnumD { id; typ; fields; _ } ->
      let mixop = [ [ atom "SEnumD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vid; vtyp; vfields ]) $$$ Ctx.note_source ()
  | NewTypeD { id; typdef; _ } ->
      let mixop = [ [ atom "NewTypeD" ]; []; [] ] in
      let vid = in_id id in
      let vtypdef = in_typdef typdef in
      CaseV (mixop, [ vid; vtypdef ]) $$$ Ctx.note_source ()
  | TypeDefD { id; typdef; _ } ->
      let mixop = [ [ atom "TypeDefD" ]; []; [] ] in
      let vid = in_id id in
      let vtypdef = in_typdef typdef in
      CaseV (mixop, [ vid; vtypdef ]) $$$ Ctx.note_source ()
  | ValueSetD { id; typ; size; _ } ->
      let mixop = [ [ atom "ValueSetD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vsize = in_expr size in
      CaseV (mixop, [ vid; vtyp; vsize ]) $$$ Ctx.note_source ()
  | ParserTypeD { id; tparams; params; _ } ->
      let mixop = [ [ atom "ParserTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtparams; vparams ]) $$$ Ctx.note_source ()
  | ParserD { id; params; cparams; locals; states; _ } ->
      let mixop = [ [ atom "ParserD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vcparams = in_cparams cparams in
      let vlocals = in_decls locals in
      let vstates = in_parser_states states in
      CaseV (mixop, [ vid; vparams; vcparams; vlocals; vstates ])
      $$$ Ctx.note_source ()
  | TableD { id; table; _ } ->
      let mixop = [ [ atom "TableD" ]; []; [] ] in
      let vid = in_id id in
      let vtable = in_table table in
      CaseV (mixop, [ vid; vtable ]) $$$ Ctx.note_source ()
  | ControlTypeD { id; tparams; params; _ } ->
      let mixop = [ [ atom "ControlTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtparams; vparams ]) $$$ Ctx.note_source ()
  | ControlD { id; params; cparams; locals; body; _ } ->
      let mixop = [ [ atom "ControlD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vcparams = in_cparams cparams in
      let vlocals = in_decls locals in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vparams; vcparams; vlocals; vbody ])
      $$$ Ctx.note_source ()
  | ActionD { id; params; body; _ } ->
      let mixop = [ [ atom "ActionD" ]; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vparams; vbody ]) $$$ Ctx.note_source ()
  | FuncD { id; typ_ret; tparams; params; body; _ } ->
      let mixop = [ [ atom "FuncD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams; vbody ])
      $$$ Ctx.note_source ()
  | ExternFuncD { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternFuncD" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ]) $$$ Ctx.note_source ()
  | ExternObjectD { id; tparams; mthds; _ } ->
      let mixop = [ [ atom "ExternObjectD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vmthds = in_mthds mthds in
      CaseV (mixop, [ vid; vtparams; vmthds ]) $$$ Ctx.note_source ()
  | PackageTypeD { id; tparams; cparams; _ } ->
      let mixop = [ [ atom "PackageTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vcparams = in_cparams cparams in
      CaseV (mixop, [ vid; vtparams; vcparams ]) $$$ Ctx.note_source ()

and in_decls (decls : P4.decl list) : value = in_list in_decl decls

(* Parser state machine *)

and in_parser_state (parser_state : P4.parser_state) : value =
  let state_label, block, _ = parser_state.it in
  let mixop = [ []; []; [] ] in
  let vstate_label = in_state_label state_label in
  let vblock = in_block block in
  CaseV (mixop, [ vstate_label; vblock ]) $$$ Ctx.note_source ()

and in_parser_states (parser_states : P4.parser_state list) : value =
  in_list in_parser_state parser_states

(* Tables *)

and in_table (table : P4.table) : value = in_list in_table_property table

(* Table properties *)

and in_table_property (table_property : P4.table_property) : value =
  match table_property with
  | KeyP table_keys ->
      let mixop = [ [ atom "KeyP" ]; [] ] in
      let vtable_keys = in_table_keys table_keys in
      CaseV (mixop, [ vtable_keys ]) $$$ Ctx.note_source ()
  | ActionP table_actions ->
      let mixop = [ [ atom "ActionP" ]; [] ] in
      let vtable_actions = in_table_actions table_actions in
      CaseV (mixop, [ vtable_actions ]) $$$ Ctx.note_source ()
  | EntryP table_entries ->
      let mixop = [ [ atom "EntryP" ]; [] ] in
      let vtable_entries = in_table_entries table_entries in
      CaseV (mixop, [ vtable_entries ]) $$$ Ctx.note_source ()
  | DefaultP table_default ->
      let mixop = [ [ atom "DefaultP" ]; [] ] in
      let vtable_default = in_table_default table_default in
      CaseV (mixop, [ vtable_default ]) $$$ Ctx.note_source ()
  | CustomP table_custom ->
      let mixop = [ [ atom "CustomP" ]; [] ] in
      let vtable_custom = in_table_custom table_custom in
      CaseV (mixop, [ vtable_custom ]) $$$ Ctx.note_source ()

(* Table keys *)

and in_table_key (table_key : P4.table_key) : value =
  let expr, match_kind, _ = table_key.it in
  let mixop = [ []; []; [] ] in
  let vexpr = in_expr expr in
  let vmatch_kind = in_match_kind match_kind in
  CaseV (mixop, [ vexpr; vmatch_kind ]) $$$ Ctx.note_source ()

and in_table_keys (table_keys : P4.table_keys) : value =
  in_list in_table_key table_keys.it

(* Table action references *)

and in_table_action (table_action : P4.table_action) : value =
  let var, args, _ = table_action.it in
  let mixop = [ []; []; [] ] in
  let vvar = in_var var in
  let vargs = in_args args in
  CaseV (mixop, [ vvar; vargs ]) $$$ Ctx.note_source ()

and in_table_actions (table_actions : P4.table_actions) : value =
  in_list in_table_action table_actions.it

(* Table entries *)

and in_table_entry (table_entry : P4.table_entry) : value =
  let table_entry_const, keysets, table_action, expr_opt, _ = table_entry.it in
  let mixop = [ []; []; []; []; [] ] in
  let vtable_entry_const = in_bool table_entry_const in
  let vkeysets = in_keysets keysets in
  let vtable_action = in_table_action table_action in
  let vexpr_opt = in_opt in_expr expr_opt in
  CaseV (mixop, [ vtable_entry_const; vkeysets; vtable_action; vexpr_opt ])
  $$$ Ctx.note_source ()

and in_table_entries (table_entries : P4.table_entries) : value =
  let table_entries_const, table_entries = table_entries.it in
  let mixop = [ []; []; [] ] in
  let vtable_entries_const = in_bool table_entries_const in
  let vtable_entries = in_list in_table_entry table_entries in
  CaseV (mixop, [ vtable_entries_const; vtable_entries ]) $$$ Ctx.note_source ()

(* Table default properties *)

and in_table_default (table_default : P4.table_default) : value =
  let table_default_const, table_action = table_default.it in
  let mixop = [ []; []; [] ] in
  let vtable_default_const = in_bool table_default_const in
  let vtable_action = in_table_action table_action in
  CaseV (mixop, [ vtable_default_const; vtable_action ]) $$$ Ctx.note_source ()

(* Table custom properties *)

and in_table_custom (table_custom : P4.table_custom) : value =
  let table_custom_const, member, expr, _ = table_custom.it in
  let mixop = [ []; []; []; [] ] in
  let vtable_custom_const = in_bool table_custom_const in
  let vmember = in_member member in
  let vexpr = in_expr expr in
  CaseV (mixop, [ vtable_custom_const; vmember; vexpr ]) $$$ Ctx.note_source ()

(* Methods *)

and in_mthd (mthd : P4.mthd) : value =
  match mthd.it with
  | ExternConsM { id; cparams; _ } ->
      let mixop = [ [ atom "ExternConsM" ]; []; [] ] in
      let vid = in_id id in
      let vcparams = in_cparams cparams in
      CaseV (mixop, [ vid; vcparams ]) $$$ Ctx.note_source ()
  | ExternAbstractM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternAbstractM" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ]) $$$ Ctx.note_source ()
  | ExternM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternM" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ]) $$$ Ctx.note_source ()

and in_mthds (mthds : P4.mthd list) : value = in_list in_mthd mthds

(* Program *)

let in_program (program : P4.program) : value = in_list in_decl program
