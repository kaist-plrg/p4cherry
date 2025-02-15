open Xl
open El.Ast
open Dom
open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error at "elab" msg

(* Todo *)

let todo (func : string) (msg : string) =
  Format.printf "(TODO : %s) %s\n" func msg;
  assert false

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let distinct (eq : 'a -> 'a -> bool) (xs : 'a list) : bool =
  let rec distinct' xs =
    match xs with
    | [] -> true
    | x :: xs -> if List.exists (eq x) xs then false else distinct' xs
  in
  distinct' xs

(* Backtracking *)

type 'a attempt = Ok of 'a | Fail of region * string

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

(* Parentheses handling *)

let rec unparen_plaintyp (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | ParenT plaintyp -> unparen_plaintyp plaintyp
  | _ -> plaintyp

let unparen_nottyp (nottyp : nottyp) : nottyp =
  match nottyp.it with
  | PlainT { it = ParenT plaintyp; at; _ } ->
      let plaintyp = unparen_plaintyp plaintyp in
      PlainT plaintyp $ at
  | _ -> nottyp

let rec unparen_exp (exp : exp) : exp =
  match exp.it with ParenE exp -> unparen_exp exp | _ -> exp

(* Iteration *)

let elab_iter (iter : iter) : Il.Ast.iter =
  match iter with Opt -> Il.Ast.Opt | List -> Il.Ast.List

(* Types *)

(* Plain types *)

let rec elab_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : Il.Ast.typ =
  let typ_il = elab_plaintyp' ctx plaintyp.it in
  typ_il $ plaintyp.at

and elab_plaintyp' (ctx : Ctx.t) (plaintyp : plaintyp') : Il.Ast.typ' =
  match plaintyp with
  | VarT (tid, targs) ->
      let td_opt = Ctx.find_typdef_opt ctx tid.it in
      check (Option.is_some td_opt) tid.at "type not defined";
      let typs_il = List.map (elab_plaintyp ctx) targs in
      let td = Option.get td_opt in
      let tparams = TypeDef.get_tparams td in
      check
        (List.length tparams = List.length targs)
        tid.at "type arguments do not match";
      Il.Ast.VarT (tid, typs_il)
  | BoolT -> Il.Ast.BoolT
  | NumT numtyp -> Il.Ast.NumT numtyp
  | TextT -> Il.Ast.TextT
  | ParenT plaintyp -> elab_plaintyp' ctx plaintyp.it
  | TupT plaintyps ->
      let typs_il = List.map (elab_plaintyp ctx) plaintyps in
      Il.Ast.TupT typs_il
  | IterT (plaintyp, iter) ->
      let typ_il = elab_plaintyp ctx plaintyp in
      let iter_il = elab_iter iter in
      Il.Ast.IterT (typ_il, iter_il)

(* Notation types *)

and elab_nottyp (ctx : Ctx.t) (nottyp : nottyp) : Il.Ast.nottyp =
  let nottyp_il = elab_nottyp' ctx nottyp.it in
  nottyp_il $ nottyp.at

and elab_nottyp' (ctx : Ctx.t) (nottyp : nottyp') : Il.Ast.nottyp' =
  match nottyp with
  | PlainT plaintyp ->
      let mixop = [ []; [] ] in
      let typ_il = elab_plaintyp ctx plaintyp in
      (mixop, [ typ_il ])
  | AtomT atom ->
      let mixop = [ [ atom ] ] in
      let typs_il = [] in
      (mixop, typs_il)
  | SeqT [] ->
      let mixop = [ [] ] in
      let typs_il = [] in
      (mixop, typs_il)
  | SeqT (nottyp :: nottyps) ->
      let mixop_h, typs_il_h = elab_nottyp' ctx nottyp.it in
      let mixop_t, typs_il_t = elab_nottyp' ctx (SeqT nottyps) in
      let mixop = Mixop.merge mixop_h mixop_t in
      let typs_il = typs_il_h @ typs_il_t in
      (mixop, typs_il)
  | InfixT (nottyp_l, atom, nottyp_r) ->
      let mixop_l, typs_il_l = elab_nottyp' ctx nottyp_l.it in
      let mixop_r, typs_il_r = elab_nottyp' ctx nottyp_r.it in
      let mixop_l = Mixop.merge mixop_l [ [ atom ] ] in
      let mixop = Mixop.merge mixop_l mixop_r in
      let typs_il = typs_il_l @ typs_il_r in
      (mixop, typs_il)
  | BrackT (atom_l, nottyp, atom_r) ->
      let mixop, typs_il = elab_nottyp' ctx nottyp.it in
      let mixop_l = Mixop.merge [ [ atom_l ] ] mixop in
      let mixop = Mixop.merge mixop_l [ [ atom_r ] ] in
      (mixop, typs_il)

(* Definition types *)

and elab_deftyp (ctx : Ctx.t) (deftyp : deftyp) : Il.Ast.deftyp =
  match deftyp.it with
  | NotationT nottyp -> elab_typ_def_notation ctx nottyp
  | StructT typfields -> elab_typ_def_struct ctx deftyp.at typfields
  | VariantT typcases -> elab_typ_def_variant ctx deftyp.at typcases

(* Notation type definitions *)

and elab_typ_def_notation (ctx : Ctx.t) (nottyp : nottyp) : Il.Ast.deftyp =
  match nottyp.it with
  | PlainT plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.AliasT typ_il $ nottyp.at
  | _ ->
      let nottyp_il = elab_nottyp ctx nottyp in
      Il.Ast.NotationT nottyp_il $ nottyp.at

(* Struct type definitions *)

and elab_typ_def_struct (ctx : Ctx.t) (at : region) (typfields : typfield list)
    : Il.Ast.deftyp =
  let typfields_il = List.map (elab_typfield ctx) typfields in
  let deftyp_il = Il.Ast.StructT typfields_il in
  deftyp_il $ at

and elab_typfield (ctx : Ctx.t) (typfield : typfield) : Il.Ast.typfield =
  let atom, plaintyps, _hints = typfield in
  let typs_il = List.map (elab_plaintyp ctx) plaintyps in
  (atom, typs_il)

(* Variant type definitions *)

and elab_typcase (ctx : Ctx.t) (typcase : typcase) : Il.Ast.typcase =
  let nottyp, _hints = typcase in
  elab_nottyp ctx nottyp

and elab_typ_def_variant (ctx : Ctx.t) (at : region) (typcases : typcase list) :
    Il.Ast.deftyp =
  let typcases_il = List.map (elab_typcase ctx) typcases in
  let mixops = typcases_il |> List.map it |> List.map fst in
  check (distinct Mixop.eq mixops) no_region "cases are ambiguous";
  let deftyp_il = Il.Ast.VariantT typcases_il in
  deftyp_il $ at

(* Expressions *)

and fail_infer (at : region) (construct : string) =
  Fail (at, "cannot infer type of " ^ construct)

and infer_exp (ctx : Ctx.t) (exp : exp) : Il.Ast.exp attempt =
  match exp.it with
  | BoolE b ->
      let exp_il = Il.Ast.BoolE b in
      let typ_il = Il.Ast.BoolT in
      Ok (exp_il $$ (exp.at, typ_il))
  | NumE (_, num) ->
      let exp_il = Il.Ast.NumE num in
      let typ_il = Il.Ast.NumT (Num.to_typ num) in
      Ok (exp_il $$ (exp.at, typ_il))
  | TextE text ->
      let exp_il = Il.Ast.TextE text in
      let typ_il = Il.Ast.TextT in
      Ok (exp_il $$ (exp.at, typ_il))
  | VarE _ -> todo "infer_exp" "VarE"
  | UnE _ -> todo "infer_exp" "UnE"
  | BinE _ -> todo "infer_exp" "BinE"
  | CmpE _ -> todo "infer_exp" "CmpE"
  | EpsE -> fail_infer exp.at "empty sequence"
  | ListE [] -> fail_infer exp.at "empty list"
  | ListE (_ :: _) -> todo "infer_exp" "ListE when homogeneous"
  | IdxE _ -> todo "infer_exp" "IdxE"
  | SliceE _ -> todo "infer_exp" "SliceE"
  | UpdE _ -> todo "infer_exp" "UpdE"
  | StrE _ -> fail_infer exp.at "record expression"
  | DotE _ -> todo "infer_exp" "DotE"
  | CommaE _ -> fail_infer exp.at "comma expression"
  | CatE _ -> todo "infer_exp" "CatE"
  | MemE _ -> todo "infer_exp" "MemE"
  | LenE _ -> todo "infer_exp" "LenE"
  | ParenE exp -> infer_exp ctx exp
  | TupE exps ->
      let* exps_il = infer_exps ctx exps in
      let exp_il = Il.Ast.TupE exps_il in
      let typs_il = List.map (fun exp_il -> exp_il.note $ exp_il.at) exps_il in
      let typ_il = Il.Ast.TupT typs_il in
      Ok (exp_il $$ (exp.at, typ_il))
  | CallE _ -> todo "infer_exp" "CallE"
  | IterE _ -> todo "infer_exp" "IterE"
  | TypE _ -> todo "infer_exp" "TypE"
  | ArithE _ -> todo "infer_exp" "ArithE"
  | AtomE _ -> fail_infer exp.at "atom"
  | SeqE [] -> fail_infer exp.at "empty sequence"
  | SeqE (_ :: _) -> todo "infer_exp" "SeqE when homogeneous"
  | InfixE _ -> fail_infer exp.at "infix expression"
  | BrackE _ -> fail_infer exp.at "bracket expression"
  | HoleE _ -> error exp.at "misplaced hole"
  | FuseE _ -> error exp.at "misplaced token concatenation"
  | UnparenE _ -> error exp.at "misplaced unparenthesize"
  | LatexE _ -> error exp.at "misplaced LaTeX literal"

and infer_exps (ctx : Ctx.t) (exps : exp list) : Il.Ast.exp list attempt =
  match exps with
  | [] -> Ok []
  | exp :: exps ->
      let* exp_il = infer_exp ctx exp in
      let* exps_il = infer_exps ctx exps in
      Ok (exp_il :: exps_il)

and elab_exp (ctx : Ctx.t) (plaintyp : plaintyp) (exp : exp) : Il.Ast.exp =
  let exp = unparen_exp exp in
  let plaintyp = unparen_plaintyp plaintyp in
  let typ_il = elab_plaintyp ctx plaintyp in
  match (typ_il.it, exp.it) with
  (* A variable type matches either a variable expression of the same name,
     or an expanded notation/struct/variant expression of the same type *)
  | VarT (tid_t, targs_il_t), VarE (tid_e, targs_e) ->
      check (tid_t.it = tid_e.it) exp.at "types do not match";
      let typ_il = Il.Ast.VarT (tid_t, targs_il_t) in
      let _typs_il_e = List.map (elab_plaintyp ctx) targs_e in
      let exp_il = Il.Ast.VarE tid_e in
      exp_il $$ (exp.at, typ_il)
  | VarT (tid, _), _ -> (
      let td = Ctx.find_typdef_opt ctx tid.it |> Option.get in
      match td with
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | NotationT nottyp ->
              let notexp_il = elab_exp_not ctx nottyp exp in
              Il.Ast.CaseE notexp_il $$ (exp.at, typ_il.it)
          | StructT _ -> todo "elab_exp" "struct"
          | VariantT typcases ->
              let notexp_il = elab_exp_variant ctx typcases exp in
              Il.Ast.CaseE notexp_il $$ (exp.at, typ_il.it))
      | _ -> error exp.at "cannot expand type")
  (* For other types, no expansion is required yet *)
  | _ -> elab_exp_plain ctx plaintyp exp

(* Plain expressions *)

and elab_exp_plain (ctx : Ctx.t) (plaintyp : plaintyp) (exp : exp) : Il.Ast.exp
    =
  match exp.it with
  | BoolE b ->
      let exp_il = Il.Ast.BoolE b in
      let typ_il = Il.Ast.BoolT in
      exp_il $$ (exp.at, typ_il)
  | NumE (_, num) ->
      let exp_il = Il.Ast.NumE num in
      let typ_il = Il.Ast.NumT (Num.to_typ num) in
      exp_il $$ (exp.at, typ_il)
  | TextE s ->
      let exp_il = Il.Ast.TextE s in
      let typ_il = Il.Ast.TextT in
      exp_il $$ (exp.at, typ_il)
  | ParenE exp ->
      let exp_il = elab_exp ctx plaintyp exp in
      let typ_il = exp_il.note in
      exp_il.it $$ (exp.at, typ_il)
  | _ -> todo "elab_exp_plain" (El.Print.string_of_exp exp)

(* Notation expressions *)

and elab_exp_not (ctx : Ctx.t) (nottyp : nottyp) (exp : exp) : Il.Ast.notexp =
  let exp = unparen_exp exp in
  let nottyp = unparen_nottyp nottyp in
  match (nottyp.it, exp.it) with
  | PlainT plaintyp, _ ->
      let mixop = [ []; [] ] in
      let exp_il = elab_exp ctx plaintyp exp in
      (mixop, [ exp_il ])
  | AtomT atom_t, AtomE atom_e ->
      check (atom_t.it = atom_e.it) exp.at "atoms do not match";
      let mixop = [ [ atom_t ] ] in
      (mixop, [])
  | SeqT [], SeqE [] ->
      let mixop = [ [] ] in
      let exps_il = [] in
      (mixop, exps_il)
  | SeqT (nottyp :: nottyps), SeqE (exp :: exps) ->
      let mixop_h, exps_il_h = elab_exp_not ctx nottyp exp in
      let mixop_t, exps_il_t =
        elab_exp_not ctx (SeqT nottyps $ nottyp.at) (SeqE exps $ exp.at)
      in
      let mixop = Mixop.merge mixop_h mixop_t in
      let exps_il = exps_il_h @ exps_il_t in
      (mixop, exps_il)
  | SeqT (_ :: _), SeqE [] -> error exp.at "omitted sequence tail"
  | SeqT [], SeqE (_ :: _) -> error exp.at "expression is not empty"
  | InfixT (nottyp_l, atom_t, nottyp_r), InfixE (exp_l, atom_e, exp_r) ->
      check (atom_t.it = atom_e.it) exp.at "atoms do not match";
      let mixop_l, exps_il_l = elab_exp_not ctx nottyp_l exp_l in
      let mixop_r, exps_il_r = elab_exp_not ctx nottyp_r exp_r in
      let mixop_l = Mixop.merge mixop_l [ [ atom_t ] ] in
      let mixop = Mixop.merge mixop_l mixop_r in
      let exps_il = exps_il_l @ exps_il_r in
      (mixop, exps_il)
  | BrackT (atom_t_l, nottyp, atom_t_r), BrackE (atom_e_l, exp, atom_e_r) ->
      check (atom_t_l.it = atom_e_l.it) exp.at "left atoms do not match";
      check (atom_t_r.it = atom_e_r.it) exp.at "right atoms do not match";
      let mixop, exps_il = elab_exp_not ctx nottyp exp in
      let mixop_l = Mixop.merge [ [ atom_t_l ] ] mixop in
      let mixop = Mixop.merge mixop_l [ [ atom_t_r ] ] in
      (mixop, exps_il)
  | _ -> error exp.at "expression does not match notation"

(* Variant expressions *)

and elab_exp_variant (ctx : Ctx.t) (typcases : typcase list) (exp : exp) :
    Il.Ast.notexp =
  let notexps_il =
    List.filter_map
      (fun (nottyp, _) ->
        try
          let notexp_il = elab_exp_not ctx nottyp exp in
          Some notexp_il
        with _ -> None)
      typcases
  in
  match notexps_il with
  | [ notexp_il ] -> notexp_il
  | [] -> error exp.at "expression does not match any case"
  | _ -> error exp.at "expression matches multiple cases"

(* Parameters *)

and elab_param (ctx : Ctx.t) (param : param) : Il.Ast.param =
  match param.it with
  | ExpP plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.ExpP typ_il $ param.at
  | DefP (id, tparams, params, plaintyp) ->
      check
        (List.map it tparams |> distinct ( = ))
        id.at "type parameters are not distinct";
      let params_il = List.map (elab_param ctx) params in
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.DefP (id, tparams, params_il, typ_il) $ param.at

(* Arguments *)

and elab_arg (ctx : Ctx.t) (param : param) (arg : arg) : Il.Ast.arg =
  match (param.it, arg.it) with
  | ExpP plaintyp, ExpA exp ->
      let exp_il = elab_exp ctx plaintyp exp in
      Il.Ast.ExpA exp_il $ arg.at
  | DefP (id_p, _, _, _), DefA id_a ->
      check (id_p.it = id_a.it) arg.at "argument does not match parameter";
      Il.Ast.DefA id_a $ arg.at
  | _ -> error arg.at "argument does not match parameter"

(* Definitions *)

let rec elab_def (ctx : Ctx.t) (def : def) : Ctx.t * Il.Ast.def option =
  let wrap_some (ctx, def) = (ctx, Some def) in
  let wrap_none ctx = (ctx, None) in
  match def.it with
  | SynD (id, tparams) -> elab_syn_def ctx id tparams |> wrap_none
  | TypD (id, tparams, deftyp, _hints) ->
      elab_typ_def ctx id tparams deftyp |> wrap_some
  | RelD (id, nottyp, _hints) -> elab_rel_def ctx def.at id nottyp |> wrap_some
  | RuleD (id_rel, id_rule, exp, prems) ->
      elab_rule_def ctx def.at id_rel id_rule exp prems |> wrap_none
  | DecD (id, tparams, params, plaintyp, _hints) ->
      elab_dec_def ctx def.at id tparams params plaintyp |> wrap_some
  | DefD (id, targs, args, exp, prems) ->
      elab_def_def ctx def.at id targs args exp prems |> wrap_none
  | SepD -> ctx |> wrap_none
  | _ -> todo "elab_def" (El.Print.string_of_def def)

and elab_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Il.Ast.def list =
  List.fold_left
    (fun (ctx, defs_il) def ->
      let ctx, def_il_opt = elab_def ctx def in
      match def_il_opt with
      | Some def_il -> (ctx, defs_il @ [ def_il ])
      | None -> (ctx, defs_il))
    (ctx, []) defs

(* Type declarations *)

and elab_syn_def (ctx : Ctx.t) (id : id) (tparams : tparam list) : Ctx.t =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  let td = TypeDef.Defining tparams in
  Ctx.add_typdef ctx id.it td

(* Type definitions *)

and elab_typ_def (ctx : Ctx.t) (id : id) (tparams : tparam list)
    (deftyp : deftyp) : Ctx.t * Il.Ast.def =
  let td_opt = Ctx.find_typdef_opt ctx id.it in
  let ctx =
    match td_opt with
    | Some (TypeDef.Defining tparams_defining) ->
        let tparams = List.map it tparams in
        let tparams_defining = List.map it tparams_defining in
        check
          (List.length tparams = List.length tparams_defining
          && List.for_all2 ( = ) tparams tparams_defining)
          id.at "type parameters do not match";
        ctx
    | None ->
        let td = TypeDef.Defining tparams in
        Ctx.add_typdef ctx id.it td
    | _ -> error id.at "type was already defined"
  in
  let ctx_local = Ctx.add_tparams ctx tparams in
  let deftyp_il = elab_deftyp ctx_local deftyp in
  let def_il = Il.Ast.TypD (id, tparams, deftyp_il) $ deftyp.at in
  let td = TypeDef.Defined (tparams, deftyp) in
  let ctx = Ctx.update_typdef ctx id.it td in
  (ctx, def_il)

(* Relation declarations *)

and elab_rel_def (ctx : Ctx.t) (at : region) (id : id) (nottyp : nottyp) :
    Ctx.t * Il.Ast.def =
  let nottyp_il = elab_nottyp ctx nottyp in
  let def_il = Il.Ast.RelD (id, nottyp_il, []) $ at in
  let ctx = Ctx.add_rel ctx id.it nottyp in
  (ctx, def_il)

(* Rule definitions *)

and elab_rule_def (ctx : Ctx.t) (at : region) (id_rel : id) (id_rule : id)
    (exp : exp) (_prems : prem list) : Ctx.t =
  let nottyp_opt = Ctx.find_rel_opt ctx id_rel.it in
  check (Option.is_some nottyp_opt) id_rel.at "relation not defined";
  let nottyp = Option.get nottyp_opt in
  let notexp_il = elab_exp_not ctx nottyp exp in
  let rule = (id_rule, notexp_il, []) $ at in
  Ctx.add_rule ctx id_rel.it rule

(* Function declarations *)

and elab_dec_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : Ctx.t * Il.Ast.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  let params_il = List.map (elab_param ctx) params in
  let typ_il = elab_plaintyp ctx plaintyp in
  let def_il = Il.Ast.DecD (id, tparams, params_il, typ_il, []) $ at in
  let ctx = Ctx.add_dec ctx id.it tparams params plaintyp in
  (ctx, def_il)

(* Function definitions *)

and elab_def_def (ctx : Ctx.t) (at : region) (id : id) (targs : plaintyp list)
    (args : arg list) (exp : exp) (_prems : prem list) : Ctx.t =
  let tparams, params, plaintyp = Ctx.find_dec_opt ctx id.it |> Option.get in
  check
    (List.length targs = List.length tparams)
    at "type arguments do not match";
  check (List.length params = List.length args) at "arguments do not match";
  let args_il = List.map2 (elab_arg ctx) params args in
  let exp_il = elab_exp ctx plaintyp exp in
  let clause = (args_il, exp_il, []) $ at in
  Ctx.add_clause ctx id.it clause

(* Spec *)

let populate_rule (ctx : Ctx.t) (def_il : Il.Ast.def) : Il.Ast.def =
  match def_il.it with
  | Il.Ast.RelD (id, nottyp_il, []) ->
      let rules_il = Ctx.find_rules_opt ctx id.it |> Option.get in
      Il.Ast.RelD (id, nottyp_il, rules_il) $ def_il.at
  | Il.Ast.RelD _ -> error def_il.at "relation was already populated"
  | _ -> def_il

let populate_rules (ctx : Ctx.t) (spec_il : Il.Ast.spec) : Il.Ast.spec =
  List.map (populate_rule ctx) spec_il

let populate_clause (ctx : Ctx.t) (def_il : Il.Ast.def) : Il.Ast.def =
  match def_il.it with
  | Il.Ast.DecD (id, tparams_il, params_il, typ_il, []) ->
      let clauses_il = Ctx.find_clauses_opt ctx id.it |> Option.get in
      Il.Ast.DecD (id, tparams_il, params_il, typ_il, clauses_il) $ def_il.at
  | Il.Ast.DecD _ -> error def_il.at "declaration was already populated"
  | _ -> def_il

let populate_clauses (ctx : Ctx.t) (spec_il : Il.Ast.spec) : Il.Ast.spec =
  List.map (populate_clause ctx) spec_il

let elab_spec (spec : spec) : Il.Ast.spec =
  let ctx = Ctx.empty in
  let ctx, spec_il = elab_defs ctx spec in
  spec_il |> populate_rules ctx |> populate_clauses ctx
