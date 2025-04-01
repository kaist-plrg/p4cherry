open Xl.Atom
module P4 = P4el.Ast
open Il.Ast
open Util.Source

type graph = Dep.Graph.t

(* Helpers *)

let in_opt (do_in : graph -> 'a -> value) (graph : graph) (opt : 'a option) :
    value =
  let vopt = Option.map (do_in graph) opt in
  let value = OptV vopt $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

let in_list (do_in : graph -> 'a -> value) (graph : graph) (lst : 'a list) :
    value =
  let vlst = List.map (do_in graph) lst in
  let value = ListV vlst $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

let in_pair (do_in_a : graph -> 'a -> value) (do_in_b : graph -> 'b -> value)
    (graph : graph) ((a, b) : 'a * 'b) : value =
  let va = do_in_a graph a in
  let vb = do_in_b graph b in
  let value = TupleV [ va; vb ] $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

let atom (s : string) : atom = Atom s $ no_region

(* Booleans *)

let in_bool (graph : graph) (boolean : bool) : value =
  let value = BoolV boolean $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* Numbers *)

let in_num (graph : graph) (num : P4.num) : value =
  match num.it with
  | i, Some (width, signed) ->
      let mixop =
        if signed then [ [ atom "FINT" ]; []; [] ]
        else [ [ atom "FBIT" ]; []; [] ]
      in
      let vwidth =
        let vwidth = NumV (`Nat width) $$$ Dep.Node.fresh () in
        Dep.Graph.add_node graph vwidth;
        vwidth
      in
      let vint =
        let vint = NumV (`Int i) $$$ Dep.Node.fresh () in
        Dep.Graph.add_node graph vint;
        vint
      in
      let value = CaseV (mixop, [ vwidth; vint ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | i, None ->
      let mixop = [ [ atom "INT" ]; [] ] in
      let vint =
        let vint = NumV (`Int i) $$$ Dep.Node.fresh () in
        Dep.Graph.add_node graph vint;
        vint
      in
      let value = CaseV (mixop, [ vint ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

(* Texts *)

let in_text (graph : graph) (text : P4.text) : value =
  let value = TextV text.it $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* Identifiers *)

let in_id (graph : graph) (id : P4.id) : value =
  let value = TextV id.it $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* Variables (scoped identifiers) *)

let in_var (graph : graph) (var : P4.var) : value =
  match var.it with
  | Top id ->
      let mixop = [ [ atom "TOP" ]; [] ] in
      let vid = in_id graph id in
      let value = CaseV (mixop, [ vid ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | Current id ->
      let mixop = [ [ atom "CURRENT" ]; [] ] in
      let vid = in_id graph id in
      let value = CaseV (mixop, [ vid ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

(* Members *)

let rec in_member (graph : graph) (member : P4.member) : value =
  let value = TextV member.it $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

and in_members (graph : graph) (members : P4.member list) : value =
  in_list in_member graph members

(* Match kinds *)

let in_match_kind (graph : graph) (match_kind : P4.match_kind) : value =
  let value = TextV match_kind.it $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* State labels *)

let in_state_label (graph : graph) (state_label : P4.state_label) : value =
  let value = TextV state_label.it $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* Unary operators *)

let in_unop (graph : graph) (unop : P4.unop) : value =
  let mixop =
    match unop.it with
    | BNotOp -> [ [ atom "BNOT" ] ]
    | LNotOp -> [ [ atom "LNOT" ] ]
    | UPlusOp -> [ [ atom "UPLUS" ] ]
    | UMinusOp -> [ [ atom "UMINUS" ] ]
  in
  let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* Binary operators *)

let in_binop (graph : graph) (binop : P4.binop) : value =
  let mixop =
    match binop.it with
    | PlusOp -> [ [ atom "PLUS" ] ]
    | SPlusOp -> [ [ atom "SPLUS" ] ]
    | MinusOp -> [ [ atom "MINUS" ] ]
    | SMinusOp -> [ [ atom "SMINUS" ] ]
    | MulOp -> [ [ atom "MUL" ] ]
    | DivOp -> [ [ atom "DIV" ] ]
    | ModOp -> [ [ atom "MOD" ] ]
    | ShlOp -> [ [ atom "SHL" ] ]
    | ShrOp -> [ [ atom "SHR" ] ]
    | LeOp -> [ [ atom "LE" ] ]
    | GeOp -> [ [ atom "GE" ] ]
    | LtOp -> [ [ atom "LT" ] ]
    | GtOp -> [ [ atom "GT" ] ]
    | EqOp -> [ [ atom "EQ" ] ]
    | NeOp -> [ [ atom "NE" ] ]
    | BAndOp -> [ [ atom "BAND" ] ]
    | BXorOp -> [ [ atom "BXOR" ] ]
    | BOrOp -> [ [ atom "BOR" ] ]
    | ConcatOp -> [ [ atom "CONCAT" ] ]
    | LAndOp -> [ [ atom "LAND" ] ]
    | LOrOp -> [ [ atom "LOR" ] ]
  in
  let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* Directions *)

let in_dir (graph : graph) (dir : P4.dir) : value =
  let mixop =
    match dir.it with
    | No -> [ [ atom "NO" ] ]
    | In -> [ [ atom "IN" ] ]
    | Out -> [ [ atom "OUT" ] ]
    | InOut -> [ [ atom "INOUT" ] ]
  in
  let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* Types *)

let rec in_typ (graph : graph) (typ : P4.typ) : value =
  match typ.it with
  | VoidT ->
      let mixop = [ [ atom "VoidT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ErrT ->
      let mixop = [ [ atom "ErrT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | MatchKindT ->
      let mixop = [ [ atom "MatchKindT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | StrT ->
      let mixop = [ [ atom "StrT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | BoolT ->
      let mixop = [ [ atom "BoolT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | IntT ->
      let mixop = [ [ atom "IntT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | FIntT expr ->
      let mixop = [ [ atom "FIntT" ]; [] ] in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | FBitT expr ->
      let mixop = [ [ atom "FBitT" ]; [] ] in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | VBitT expr ->
      let mixop = [ [ atom "VBitT" ]; [] ] in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | StackT (typ, expr) ->
      let mixop = [ [ atom "StackT" ]; []; [] ] in
      let vtyp = in_typ graph typ in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vtyp; vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ListT typ ->
      let mixop = [ [ atom "ListT" ]; [] ] in
      let vtyp = in_typ graph typ in
      let value = CaseV (mixop, [ vtyp ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | TupleT typs ->
      let mixop = [ [ atom "TupleT" ]; [] ] in
      let vtyps = in_typs graph typs in
      let value = CaseV (mixop, [ vtyps ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | NameT var ->
      let mixop = [ [ atom "NameT" ]; [] ] in
      let vvar = in_var graph var in
      let value = CaseV (mixop, [ vvar ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | SpecT (var, targs) ->
      let mixop = [ [ atom "SpecT" ]; []; [] ] in
      let vvar = in_var graph var in
      let vtargs = in_targs graph targs in
      let value = CaseV (mixop, [ vvar; vtargs ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | AnyT ->
      let mixop = [ [ atom "AnyT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

and in_typs (graph : graph) (typs : P4.typ list) : value =
  in_list in_typ graph typs

(* Type parameters *)

and in_tparam (graph : graph) (tparam : P4.tparam) : value =
  let value = TextV tparam.it $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

and in_tparams (graph : graph) (tparams : P4.tparam list) : value =
  in_list in_tparam graph tparams

(* Parameters *)

and in_param (graph : graph) (param : P4.param) : value =
  let id, dir, typ, expr_opt, _ = param.it in
  let mixop = [ []; []; []; []; [] ] in
  let vid = in_id graph id in
  let vdir = in_dir graph dir in
  let vtyp = in_typ graph typ in
  let vexpr_opt = in_opt in_expr graph expr_opt in
  let value =
    CaseV (mixop, [ vid; vdir; vtyp; vexpr_opt ]) $$$ Dep.Node.fresh ()
  in
  Dep.Graph.add_node graph value;
  value

and in_params (graph : graph) (params : P4.param list) : value =
  in_list in_param graph params

(* Constructor parameters *)

and in_cparam (graph : graph) (cparam : P4.cparam) : value =
  in_param graph cparam

and in_cparams (graph : graph) (cparams : P4.cparam list) : value =
  in_list in_cparam graph cparams

(* Type arguments *)

and in_targ (graph : graph) (targ : P4.targ) : value = in_typ graph targ

and in_targs (graph : graph) (targs : P4.targ list) : value =
  in_list in_targ graph targs

(* Arguments *)

and in_arg (graph : graph) (arg : P4.arg) : value =
  match arg.it with
  | ExprA expr ->
      let mixop = [ [ atom "ExprA" ]; [] ] in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | NameA (id, expr_opt) ->
      let mixop = [ [ atom "NameA" ]; []; [] ] in
      let vid = in_id graph id in
      let vexpr_opt = in_opt in_expr graph expr_opt in
      let value = CaseV (mixop, [ vid; vexpr_opt ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | AnyA ->
      let mixop = [ [ atom "AnyA" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

and in_args (graph : graph) (args : P4.arg list) : value =
  in_list in_arg graph args

(* Expressions *)

and in_expr (graph : graph) (expr : P4.expr) : value =
  match expr.it with
  | BoolE { boolean } ->
      let mixop = [ [ atom "BoolE" ]; [] ] in
      let vboolean = in_bool graph boolean in
      let value = CaseV (mixop, [ vboolean ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | StrE { text } ->
      let mixop = [ [ atom "StrE" ]; [] ] in
      let vtext = in_text graph text in
      let value = CaseV (mixop, [ vtext ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | NumE { num } ->
      let mixop = [ [ atom "NumE" ]; [] ] in
      let vnum = in_num graph num in
      let value = CaseV (mixop, [ vnum ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | VarE { var } ->
      let mixop = [ [ atom "NameE" ]; [] ] in
      let vvar = in_var graph var in
      let value = CaseV (mixop, [ vvar ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | SeqE { exprs } ->
      let mixop = [ [ atom "SeqE" ]; [] ] in
      let vexprs = in_exprs graph exprs in
      let value = CaseV (mixop, [ vexprs ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | SeqDefaultE { exprs } ->
      let mixop = [ [ atom "SeqDefaultE" ]; [] ] in
      let vexprs = in_exprs graph exprs in
      let value = CaseV (mixop, [ vexprs ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | RecordE { fields } ->
      let mixop = [ [ atom "RecordE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) graph fields in
      let value = CaseV (mixop, [ vfields ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | RecordDefaultE { fields } ->
      let mixop = [ [ atom "RecordDefaultE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) graph fields in
      let value = CaseV (mixop, [ vfields ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | DefaultE ->
      let mixop = [ [ atom "DefaultE" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | InvalidE ->
      let mixop = [ [ atom "InvalidE" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | UnE { unop; expr } ->
      let mixop = [ [ atom "UnE" ]; []; [] ] in
      let vunop = in_unop graph unop in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vunop; vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | BinE { binop; expr_l; expr_r } ->
      let mixop = [ [ atom "BinE" ]; []; []; [] ] in
      let vbinop = in_binop graph binop in
      let vexpr_l = in_expr graph expr_l in
      let vexpr_r = in_expr graph expr_r in
      let value =
        CaseV (mixop, [ vbinop; vexpr_l; vexpr_r ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | TernE { expr_cond; expr_then; expr_else } ->
      let mixop = [ [ atom "TernE" ]; []; []; [] ] in
      let vexpr_cond = in_expr graph expr_cond in
      let vexpr_then = in_expr graph expr_then in
      let vexpr_else = in_expr graph expr_else in
      let value =
        CaseV (mixop, [ vexpr_cond; vexpr_then; vexpr_else ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | CastE { typ; expr } ->
      let mixop = [ [ atom "CastE" ]; []; [] ] in
      let vtyp = in_typ graph typ in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vtyp; vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | MaskE { expr_base; expr_mask } ->
      let mixop = [ [ atom "MaskE" ]; []; [] ] in
      let vexpr_base = in_expr graph expr_base in
      let vexpr_mask = in_expr graph expr_mask in
      let value =
        CaseV (mixop, [ vexpr_base; vexpr_mask ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | RangeE { expr_lb; expr_ub } ->
      let mixop = [ [ atom "RangeE" ]; []; [] ] in
      let vexpr_lb = in_expr graph expr_lb in
      let vexpr_ub = in_expr graph expr_ub in
      let value = CaseV (mixop, [ vexpr_lb; vexpr_ub ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | SelectE { exprs_select; cases } ->
      let mixop = [ [ atom "SelectE" ]; []; [] ] in
      let vexprs_select = in_exprs graph exprs_select in
      let vcases = in_select_cases graph cases in
      let value =
        CaseV (mixop, [ vexprs_select; vcases ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | ArrAccE { expr_base; expr_idx } ->
      let mixop = [ [ atom "ArrAccE" ]; []; [] ] in
      let vexpr_base = in_expr graph expr_base in
      let vexpr_idx = in_expr graph expr_idx in
      let value =
        CaseV (mixop, [ vexpr_base; vexpr_idx ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      let mixop = [ [ atom "BitAccE" ]; []; []; [] ] in
      let vexpr_base = in_expr graph expr_base in
      let vexpr_lo = in_expr graph expr_lo in
      let vexpr_hi = in_expr graph expr_hi in
      let value =
        CaseV (mixop, [ vexpr_base; vexpr_lo; vexpr_hi ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | ErrAccE { member } ->
      let mixop = [ [ atom "ErrAccE" ]; [] ] in
      let vmember = in_member graph member in
      let value = CaseV (mixop, [ vmember ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | TypeAccE { var_base; member } ->
      let mixop = [ [ atom "TypeAccE" ]; []; [] ] in
      let vvar_base = in_var graph var_base in
      let vmember = in_member graph member in
      let value = CaseV (mixop, [ vvar_base; vmember ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ExprAccE { expr_base; member } ->
      let mixop = [ [ atom "ExprAccE" ]; []; [] ] in
      let vexpr_base = in_expr graph expr_base in
      let vmember = in_member graph member in
      let value =
        CaseV (mixop, [ vexpr_base; vmember ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | CallFuncE { var_func; targs; args } ->
      let mixop = [ [ atom "CallFuncE" ]; []; []; [] ] in
      let vvar_func = in_var graph var_func in
      let vtargs = in_targs graph targs in
      let vargs = in_args graph args in
      let value =
        CaseV (mixop, [ vvar_func; vtargs; vargs ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | CallMethodE { expr_base; member; targs; args } ->
      let mixop = [ [ atom "CallMethodE" ]; []; []; []; [] ] in
      let vexpr_base = in_expr graph expr_base in
      let vmember = in_member graph member in
      let vtargs = in_targs graph targs in
      let vargs = in_args graph args in
      let value =
        CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | CallTypeE { var_typ; member; targs; args } ->
      let mixop = [ [ atom "CallTypeE" ]; []; []; []; [] ] in
      let vvar_typ = in_var graph var_typ in
      let vmember = in_member graph member in
      let vtargs = in_targs graph targs in
      let vargs = in_args graph args in
      let value =
        CaseV (mixop, [ vvar_typ; vmember; vtargs; vargs ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | InstE { var_inst; targs; args } ->
      let mixop = [ [ atom "InstE" ]; []; []; [] ] in
      let vvar_inst = in_var graph var_inst in
      let vtargs = in_targs graph targs in
      let vargs = in_args graph args in
      let value =
        CaseV (mixop, [ vvar_inst; vtargs; vargs ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value

and in_exprs (graph : graph) (exprs : P4.expr list) : value =
  in_list in_expr graph exprs

(* Keyset expressions *)

and in_keyset (graph : graph) (keyset : P4.keyset) : value =
  match keyset.it with
  | ExprK expr ->
      let mixop = [ [ atom "ExprK" ]; [] ] in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | DefaultK ->
      let mixop = [ [ atom "DefaultK" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | AnyK ->
      let mixop = [ [ atom "AnyK" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

and in_keysets (graph : graph) (keysets : P4.keyset list) : value =
  in_list in_keyset graph keysets

(* Select-cases for select *)

and in_select_case (graph : graph) (select_case : P4.select_case) : value =
  let keysets, state_label = select_case.it in
  let mixop = [ []; []; [] ] in
  let vkeysets = in_keysets graph keysets in
  let vstate_label = in_state_label graph state_label in
  let value = CaseV (mixop, [ vkeysets; vstate_label ]) $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

and in_select_cases (graph : graph) (select_cases : P4.select_case list) : value
    =
  in_list in_select_case graph select_cases

(* Statements *)

and in_stmt (graph : graph) (stmt : P4.stmt) : value =
  match stmt.it with
  | EmptyS ->
      let mixop = [ [ atom "EmptyS" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | AssignS { expr_l; expr_r } ->
      let mixop = [ [ atom "AssignS" ]; []; [] ] in
      let vexpr_l = in_expr graph expr_l in
      let vexpr_r = in_expr graph expr_r in
      let value = CaseV (mixop, [ vexpr_l; vexpr_r ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | SwitchS { expr_switch; cases } ->
      let mixop = [ [ atom "SwitchS" ]; []; [] ] in
      let vexpr_switch = in_expr graph expr_switch in
      let vcases = in_switch_cases graph cases in
      let value =
        CaseV (mixop, [ vexpr_switch; vcases ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | IfS { expr_cond; stmt_then; stmt_else } ->
      let mixop = [ [ atom "IfS" ]; []; []; [] ] in
      let vexpr_cond = in_expr graph expr_cond in
      let vstmt_then = in_stmt graph stmt_then in
      let vstmt_else = in_stmt graph stmt_else in
      let value =
        CaseV (mixop, [ vexpr_cond; vstmt_then; vstmt_else ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | BlockS { block } ->
      let mixop = [ [ atom "BlockS" ]; [] ] in
      let vblock = in_block graph block in
      let value = CaseV (mixop, [ vblock ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ExitS ->
      let mixop = [ [ atom "ExitS" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | RetS { expr_ret } ->
      let mixop = [ [ atom "RetS" ]; [] ] in
      let vexpr_ret = in_opt in_expr graph expr_ret in
      let value = CaseV (mixop, [ vexpr_ret ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | CallFuncS { var_func; targs; args } ->
      let mixop = [ [ atom "CallFuncS" ]; []; []; [] ] in
      let vvar_func = in_var graph var_func in
      let vtargs = in_targs graph targs in
      let vargs = in_args graph args in
      let value =
        CaseV (mixop, [ vvar_func; vtargs; vargs ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | CallMethodS { expr_base; member; targs; args } ->
      let mixop = [ [ atom "CallMethodS" ]; []; []; []; [] ] in
      let vexpr_base = in_expr graph expr_base in
      let vmember = in_member graph member in
      let vtargs = in_targs graph targs in
      let vargs = in_args graph args in
      let value =
        CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | CallInstS { var_inst; targs; args } ->
      let mixop = [ [ atom "CallInstS" ]; []; []; [] ] in
      let vvar_inst = in_var graph var_inst in
      let vtargs = in_targs graph targs in
      let vargs = in_args graph args in
      let value =
        CaseV (mixop, [ vvar_inst; vtargs; vargs ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | TransS { expr_label } ->
      let mixop = [ [ atom "TransS" ]; [] ] in
      let vexpr_label = in_expr graph expr_label in
      let value = CaseV (mixop, [ vexpr_label ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | DeclS { decl } ->
      let mixop = [ [ atom "DeclS" ]; [] ] in
      let vdecl = in_decl graph decl in
      let value = CaseV (mixop, [ vdecl ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

and in_stmts (graph : graph) (stmts : P4.stmt list) : value =
  in_list in_stmt graph stmts

(* Blocks (sequence of statements) *)

and in_block (graph : graph) (block : P4.block) : value =
  let stmts, _ = block.it in
  let mixop = [ [ atom "BlockB" ]; [] ] in
  let vstmts = in_stmts graph stmts in
  let value = CaseV (mixop, [ vstmts ]) $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

(* Match-cases for switch *)

and in_switch_label (graph : graph) (switch_label : P4.switch_label) : value =
  match switch_label.it with
  | ExprL expr ->
      let mixop = [ [ atom "ExprL" ]; [] ] in
      let vexpr = in_expr graph expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | DefaultL ->
      let mixop = [ [ atom "DefaultL" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

and in_switch_case (graph : graph) (switch_case : P4.switch_case) : value =
  match switch_case.it with
  | MatchC (switch_label, block) ->
      let mixop = [ [ atom "MatchC" ]; []; [] ] in
      let vswitch_label = in_switch_label graph switch_label in
      let vblock = in_block graph block in
      let value =
        CaseV (mixop, [ vswitch_label; vblock ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | FallC switch_label ->
      let mixop = [ [ atom "FallC" ]; [] ] in
      let vswitch_label = in_switch_label graph switch_label in
      let value = CaseV (mixop, [ vswitch_label ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

and in_switch_cases (graph : graph) (switch_cases : P4.switch_case list) : value
    =
  in_list in_switch_case graph switch_cases

(* Declarations *)

and in_typdef (graph : graph) (typdef : (P4.typ, P4.decl) P4.alt) : value =
  match typdef with
  | Left typ ->
      let mixop = [ [ atom "TypeD" ]; [] ] in
      let vtyp = in_typ graph typ in
      let value = CaseV (mixop, [ vtyp ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | Right decl ->
      let mixop = [ [ atom "DeclD" ]; [] ] in
      let vdecl = in_decl graph decl in
      let value = CaseV (mixop, [ vdecl ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

and in_decl (graph : graph) (decl : P4.decl) : value =
  match decl.it with
  | ConstD { id; typ; value; _ } ->
      let mixop = [ [ atom "ConstD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtyp = in_typ graph typ in
      let vvalue = in_expr graph value in
      let value = CaseV (mixop, [ vid; vtyp; vvalue ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | VarD { id; typ; init; _ } ->
      let mixop = [ [ atom "VarD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtyp = in_typ graph typ in
      let vinit = in_opt in_expr graph init in
      let value = CaseV (mixop, [ vid; vtyp; vinit ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ErrD { members } ->
      let mixop = [ [ atom "ErrD" ]; [] ] in
      let vmembers = in_members graph members in
      let value = CaseV (mixop, [ vmembers ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | MatchKindD { members } ->
      let mixop = [ [ atom "MatchKindD" ]; [] ] in
      let vmembers = in_members graph members in
      let value = CaseV (mixop, [ vmembers ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | InstD { id; var_inst; targs; args; init; _ } ->
      let mixop = [ [ atom "InstD" ]; []; []; []; []; [] ] in
      let vid = in_id graph id in
      let vvar_inst = in_var graph var_inst in
      let vtargs = in_targs graph targs in
      let vargs = in_args graph args in
      let vinit = in_decls graph init in
      let value =
        CaseV (mixop, [ vid; vvar_inst; vtargs; vargs; vinit ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | StructD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "StructD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtparams = in_tparams graph tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) graph fields
      in
      let value =
        CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | HeaderD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "HeaderD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtparams = in_tparams graph tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) graph fields
      in
      let value =
        CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | UnionD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "UnionD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtparams = in_tparams graph tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) graph fields
      in
      let value =
        CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | EnumD { id; members; _ } ->
      let mixop = [ [ atom "EnumD" ]; []; [] ] in
      let vid = in_id graph id in
      let vmembers = in_members graph members in
      let value = CaseV (mixop, [ vid; vmembers ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | SEnumD { id; typ; fields; _ } ->
      let mixop = [ [ atom "SEnumD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtyp = in_typ graph typ in
      let vfields = in_list (in_pair in_member in_expr) graph fields in
      let value = CaseV (mixop, [ vid; vtyp; vfields ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | NewTypeD { id; typdef; _ } ->
      let mixop = [ [ atom "NewTypeD" ]; []; [] ] in
      let vid = in_id graph id in
      let vtypdef = in_typdef graph typdef in
      let value = CaseV (mixop, [ vid; vtypdef ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | TypeDefD { id; typdef; _ } ->
      let mixop = [ [ atom "TypeDefD" ]; []; [] ] in
      let vid = in_id graph id in
      let vtypdef = in_typdef graph typdef in
      let value = CaseV (mixop, [ vid; vtypdef ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ValueSetD { id; typ; size; _ } ->
      let mixop = [ [ atom "ValueSetD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtyp = in_typ graph typ in
      let vsize = in_expr graph size in
      let value = CaseV (mixop, [ vid; vtyp; vsize ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ParserTypeD { id; tparams; params; _ } ->
      let mixop = [ [ atom "ParserTypeD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtparams = in_tparams graph tparams in
      let vparams = in_params graph params in
      let value =
        CaseV (mixop, [ vid; vtparams; vparams ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | ParserD { id; params; cparams; locals; states; _ } ->
      let mixop = [ [ atom "ParserD" ]; []; []; []; []; [] ] in
      let vid = in_id graph id in
      let vparams = in_params graph params in
      let vcparams = in_cparams graph cparams in
      let vlocals = in_decls graph locals in
      let vstates = in_parser_states graph states in
      let value =
        CaseV (mixop, [ vid; vparams; vcparams; vlocals; vstates ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | TableD { id; table; _ } ->
      let mixop = [ [ atom "TableD" ]; []; [] ] in
      let vid = in_id graph id in
      let vtable = in_table graph table in
      let value = CaseV (mixop, [ vid; vtable ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ControlTypeD { id; tparams; params; _ } ->
      let mixop = [ [ atom "ControlTypeD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtparams = in_tparams graph tparams in
      let vparams = in_params graph params in
      let value =
        CaseV (mixop, [ vid; vtparams; vparams ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | ControlD { id; params; cparams; locals; body; _ } ->
      let mixop = [ [ atom "ControlD" ]; []; []; []; []; [] ] in
      let vid = in_id graph id in
      let vparams = in_params graph params in
      let vcparams = in_cparams graph cparams in
      let vlocals = in_decls graph locals in
      let vbody = in_block graph body in
      let value =
        CaseV (mixop, [ vid; vparams; vcparams; vlocals; vbody ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | ActionD { id; params; body; _ } ->
      let mixop = [ [ atom "ActionD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vparams = in_params graph params in
      let vbody = in_block graph body in
      let value =
        CaseV (mixop, [ vid; vparams; vbody ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | FuncD { id; typ_ret; tparams; params; body; _ } ->
      let mixop = [ [ atom "FuncD" ]; []; []; []; []; [] ] in
      let vid = in_id graph id in
      let vtyp_ret = in_typ graph typ_ret in
      let vtparams = in_tparams graph tparams in
      let vparams = in_params graph params in
      let vbody = in_block graph body in
      let value =
        CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams; vbody ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | ExternFuncD { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternFuncD" ]; []; []; []; [] ] in
      let vid = in_id graph id in
      let vtyp_ret = in_typ graph typ_ret in
      let vtparams = in_tparams graph tparams in
      let vparams = in_params graph params in
      let value =
        CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | ExternObjectD { id; tparams; mthds; _ } ->
      let mixop = [ [ atom "ExternObjectD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtparams = in_tparams graph tparams in
      let vmthds = in_mthds graph mthds in
      let value =
        CaseV (mixop, [ vid; vtparams; vmthds ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | PackageTypeD { id; tparams; cparams; _ } ->
      let mixop = [ [ atom "PackageTypeD" ]; []; []; [] ] in
      let vid = in_id graph id in
      let vtparams = in_tparams graph tparams in
      let vcparams = in_cparams graph cparams in
      let value =
        CaseV (mixop, [ vid; vtparams; vcparams ]) $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value

and in_decls (graph : graph) (decls : P4.decl list) : value =
  in_list in_decl graph decls

(* Parser state machine *)

and in_parser_state (graph : graph) (parser_state : P4.parser_state) : value =
  let state_label, block, _ = parser_state.it in
  let mixop = [ []; []; [] ] in
  let vstate_label = in_state_label graph state_label in
  let vblock = in_block graph block in
  let value = CaseV (mixop, [ vstate_label; vblock ]) $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

and in_parser_states (graph : graph) (parser_states : P4.parser_state list) :
    value =
  in_list in_parser_state graph parser_states

(* Tables *)

and in_table (graph : graph) (table : P4.table) : value =
  in_list in_table_property graph table

(* Table properties *)

and in_table_property (graph : graph) (table_property : P4.table_property) :
    value =
  match table_property with
  | KeyP table_keys ->
      let mixop = [ [ atom "KeyP" ]; [] ] in
      let vtable_keys = in_table_keys graph table_keys in
      let value = CaseV (mixop, [ vtable_keys ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ActionP table_actions ->
      let mixop = [ [ atom "ActionP" ]; [] ] in
      let vtable_actions = in_table_actions graph table_actions in
      let value = CaseV (mixop, [ vtable_actions ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | EntryP table_entries ->
      let mixop = [ [ atom "EntryP" ]; [] ] in
      let vtable_entries = in_table_entries graph table_entries in
      let value = CaseV (mixop, [ vtable_entries ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | DefaultP table_default ->
      let mixop = [ [ atom "DefaultP" ]; [] ] in
      let vtable_default = in_table_default graph table_default in
      let value = CaseV (mixop, [ vtable_default ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | CustomP table_custom ->
      let mixop = [ [ atom "CustomP" ]; [] ] in
      let vtable_custom = in_table_custom graph table_custom in
      let value = CaseV (mixop, [ vtable_custom ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value

(* Table keys *)

and in_table_key (graph : graph) (table_key : P4.table_key) : value =
  let expr, match_kind, _ = table_key.it in
  let mixop = [ []; []; [] ] in
  let vexpr = in_expr graph expr in
  let vmatch_kind = in_match_kind graph match_kind in
  let value = CaseV (mixop, [ vexpr; vmatch_kind ]) $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

and in_table_keys (graph : graph) (table_keys : P4.table_keys) : value =
  in_list in_table_key graph table_keys.it

(* Table action references *)

and in_table_action (graph : graph) (table_action : P4.table_action) : value =
  let var, args, _ = table_action.it in
  let mixop = [ []; []; [] ] in
  let vvar = in_var graph var in
  let vargs = in_args graph args in
  let value = CaseV (mixop, [ vvar; vargs ]) $$$ Dep.Node.fresh () in
  Dep.Graph.add_node graph value;
  value

and in_table_actions (graph : graph) (table_actions : P4.table_actions) : value
    =
  in_list in_table_action graph table_actions.it

(* Table entries *)

and in_table_entry (graph : graph) (table_entry : P4.table_entry) : value =
  let table_entry_const, keysets, table_action, expr_opt, _ = table_entry.it in
  let mixop = [ []; []; []; []; [] ] in
  let vtable_entry_const = in_bool graph table_entry_const in
  let vkeysets = in_keysets graph keysets in
  let vtable_action = in_table_action graph table_action in
  let vexpr_opt = in_opt in_expr graph expr_opt in
  let value =
    CaseV (mixop, [ vtable_entry_const; vkeysets; vtable_action; vexpr_opt ])
    $$$ Dep.Node.fresh ()
  in
  Dep.Graph.add_node graph value;
  value

and in_table_entries (graph : graph) (table_entries : P4.table_entries) : value
    =
  let table_entries_const, table_entries = table_entries.it in
  let mixop = [ []; []; [] ] in
  let vtable_entries_const = in_bool graph table_entries_const in
  let vtable_entries = in_list in_table_entry graph table_entries in
  let value =
    CaseV (mixop, [ vtable_entries_const; vtable_entries ])
    $$$ Dep.Node.fresh ()
  in
  Dep.Graph.add_node graph value;
  value

(* Table default properties *)

and in_table_default (graph : graph) (table_default : P4.table_default) : value
    =
  let table_default_const, table_action = table_default.it in
  let mixop = [ []; []; [] ] in
  let vtable_default_const = in_bool graph table_default_const in
  let vtable_action = in_table_action graph table_action in
  let value =
    CaseV (mixop, [ vtable_default_const; vtable_action ]) $$$ Dep.Node.fresh ()
  in
  Dep.Graph.add_node graph value;
  value

(* Table custom properties *)

and in_table_custom (graph : graph) (table_custom : P4.table_custom) : value =
  let table_custom_const, member, expr, _ = table_custom.it in
  let mixop = [ []; []; []; [] ] in
  let vtable_custom_const = in_bool graph table_custom_const in
  let vmember = in_member graph member in
  let vexpr = in_expr graph expr in
  let value =
    CaseV (mixop, [ vtable_custom_const; vmember; vexpr ]) $$$ Dep.Node.fresh ()
  in
  Dep.Graph.add_node graph value;
  value

(* Methods *)

and in_mthd (graph : graph) (mthd : P4.mthd) : value =
  match mthd.it with
  | ExternConsM { id; cparams; _ } ->
      let mixop = [ [ atom "ExternConsM" ]; []; [] ] in
      let vid = in_id graph id in
      let vcparams = in_cparams graph cparams in
      let value = CaseV (mixop, [ vid; vcparams ]) $$$ Dep.Node.fresh () in
      Dep.Graph.add_node graph value;
      value
  | ExternAbstractM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternAbstractM" ]; []; []; []; [] ] in
      let vid = in_id graph id in
      let vtyp_ret = in_typ graph typ_ret in
      let vtparams = in_tparams graph tparams in
      let vparams = in_params graph params in
      let value =
        CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value
  | ExternM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternM" ]; []; []; []; [] ] in
      let vid = in_id graph id in
      let vtyp_ret = in_typ graph typ_ret in
      let vtparams = in_tparams graph tparams in
      let vparams = in_params graph params in
      let value =
        CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
        $$$ Dep.Node.fresh ()
      in
      Dep.Graph.add_node graph value;
      value

and in_mthds (graph : graph) (mthds : P4.mthd list) : value =
  in_list in_mthd graph mthds

(* Program *)

let in_program (graph : graph) (program : P4.program) : value =
  in_list in_decl graph program
