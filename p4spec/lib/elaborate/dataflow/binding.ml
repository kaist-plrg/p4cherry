open Domain.Lib
open Il.Ast
module Typ = Runtime_static.Typ
open Runtime_static.Rel
open Runtime_static.Envs
open Error
open Bind
open Util.Source

(* Binding analysis :

   1. Collect all binding occurrences of variables in IL construct
      - Check that all binding occurrences reside in invertible constructs
   2. Rename multi/parallel binding occurrences
      - e.g., -- let (int, int) = ... becomes
                -- let (int, int') = ..., -- if int = int'
   3. Desugar partial bindings
      - e.g., -- let (int, 1 + 2) = ... becomes
              -- let (int, int') = ..., -- if int' = 1 + 2
   Note. At this point, binder patterns contain binding identifiers only
   4. Desugar patterned bindings (including downcasts)
      - e.g., -- let (int, CASE int' int'') = ... becomes
              -- let (int, case) = ..., -- if case matches CASE, -- let CASE int' int'' = case
      - e.g., -- let ((typ) child) = ... becomes
              -- let parent = ..., -- if parent <: child, -- let child = parent *)

let update_venv_multi (venv : VEnv.t) (renv_multi : Multibind.REnv.t) : VEnv.t =
  Multibind.REnv.fold
    (fun id ids_rename venv ->
      let ids_rename = IdSet.elements ids_rename in
      let typ = VEnv.find id venv in
      List.fold_left
        (fun venv id_rename -> VEnv.add id_rename typ venv)
        venv ids_rename)
    renv_multi venv

let update_venv_partial (venv : VEnv.t) (renv_partial : Partialbind.REnv.t) :
    VEnv.t =
  Partialbind.REnv.fold
    (fun id (exp, iters) venv -> VEnv.add id (exp.note $ exp.at, iters) venv)
    renv_partial venv

let update_venv_inject (venv : VEnv.t) (renv_inject : Injectbind.REnv.t) :
    VEnv.t =
  List.fold_left
    (fun venv (id, (exp, _, iters)) ->
      VEnv.add id (exp.note $ exp.at, iters) venv)
    venv renv_inject

(* Expression binding analysis *)

let analyze_exps_as_bind (dctx : Dctx.t) (exps : exp list) :
    Dctx.t * VEnv.t * exp list * prem list =
  let binds = Collectbind.collect_exps dctx exps in
  let venv = BEnv.flatten binds in
  let dctx, renv_multi, exps =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_exps dctx renv_multi exps
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.REnv.gen_sideconditions binds renv_multi
  in
  let dctx, renv_partial, exps =
    Partialbind.rename_exps dctx (VEnv.dom venv) Partialbind.REnv.empty exps
  in
  let venv = update_venv_partial venv renv_partial in
  let sideconditions_partial =
    Partialbind.REnv.gen_sideconditions renv_partial
  in
  let dctx, renv_inject, exps =
    Injectbind.rename_exps dctx Injectbind.REnv.empty exps
  in
  let venv = update_venv_inject venv renv_inject in
  let prems_inject = Injectbind.REnv.gen_prems renv_inject in
  let prems = prems_inject @ sideconditions_multi @ sideconditions_partial in
  (dctx, venv, exps, prems)

let analyze_exp_as_bound (dctx : Dctx.t) (exp : exp) : unit =
  let binds = Collectbind.collect_exp dctx exp in
  if not (BEnv.is_empty binds) then
    error exp.at
      (Format.asprintf "expression has free variable(s): %s"
         (BEnv.to_string binds))

let analyze_exps_as_bound (dctx : Dctx.t) (exps : exp list) : unit =
  List.iter (analyze_exp_as_bound dctx) exps

(* Argument binding analysis *)

let analyze_args_as_bind (dctx : Dctx.t) (args : arg list) :
    Dctx.t * VEnv.t * arg list * prem list =
  let binds = Collectbind.collect_args dctx args in
  let venv = BEnv.flatten binds in
  let dctx, renv_multi, args =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_args dctx renv_multi args
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.REnv.gen_sideconditions binds renv_multi
  in
  let dctx, renv_partial, args =
    Partialbind.rename_args dctx (VEnv.dom venv) Partialbind.REnv.empty args
  in
  let venv = update_venv_partial venv renv_partial in
  let sideconditions_partial =
    Partialbind.REnv.gen_sideconditions renv_partial
  in
  let dctx, renv_inject, args =
    Injectbind.rename_args dctx Injectbind.REnv.empty args
  in
  let venv = update_venv_inject venv renv_inject in
  let prems_inject = Injectbind.REnv.gen_prems renv_inject in
  let prems = prems_inject @ sideconditions_multi @ sideconditions_partial in
  (dctx, venv, args, prems)

(* Premise binding analysis *)

let rec analyze_prem (dctx : Dctx.t) (prem : prem) :
    Dctx.t * VEnv.t * prem * prem list =
  match prem.it with
  | RulePr (id, notexp) -> analyze_rule_prem dctx prem.at id notexp
  | IfPr exp -> analyze_if_prem dctx prem.at exp
  | ElsePr -> (dctx, VEnv.empty, prem, [])
  | LetPr _ ->
      error prem.at "let premise should appear only after bind analysis"
  | IterPr (_, ((_, _ :: _) as iterexp)) ->
      error prem.at
        (Format.asprintf
           "iterated premise should initially have no annotations, but got %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterPr (prem, (iter, [])) -> analyze_iter_prem dctx prem.at prem iter

and analyze_rule_prem (dctx : Dctx.t) (at : region) (id : id) (notexp : notexp)
    : Dctx.t * VEnv.t * prem * prem list =
  let mixop, exps = notexp in
  let hint = Dctx.find_hint dctx id in
  let exps_input, exps_output = Hint.split_exps hint exps in
  List.map snd exps_input |> analyze_exps_as_bound dctx;
  let dctx, venv, exps_output, sideconditions =
    let idxs, exps_output = List.split exps_output in
    let dctx, venv, exps_output, sideconditions =
      analyze_exps_as_bind dctx exps_output
    in
    let exps_output = List.combine idxs exps_output in
    (dctx, venv, exps_output, sideconditions)
  in
  let exps = Hint.combine_exps exps_input exps_output in
  let notexp = (mixop, exps) in
  let prem = RulePr (id, notexp) $ at in
  (dctx, venv, prem, sideconditions)

and analyze_if_eq_prem (dctx : Dctx.t) (at : region) (note : typ')
    (optyp : optyp) (exp_l : exp) (exp_r : exp) :
    Dctx.t * VEnv.t * prem' * prem list =
  let binds_l = Collectbind.collect_exp dctx exp_l in
  let binds_r = Collectbind.collect_exp dctx exp_r in
  match (BEnv.is_empty binds_l, BEnv.is_empty binds_r) with
  | true, true ->
      let prem = IfPr (CmpE (`EqOp, optyp, exp_l, exp_r) $$ (at, note)) in
      (dctx, VEnv.empty, prem, [])
  | false, true -> analyze_let_prem dctx exp_l binds_l exp_r
  | true, false -> analyze_let_prem dctx exp_r binds_r exp_l
  | false, false ->
      error at
        (Format.asprintf
           "cannot bind on both sides of an equality: (left) %s, (right) %s"
           (BEnv.to_string binds_l) (BEnv.to_string binds_r))

and analyze_if_prem (dctx : Dctx.t) (at : region) (exp : exp) :
    Dctx.t * VEnv.t * prem * prem list =
  match exp.it with
  | CmpE (`EqOp, optyp, exp_l, exp_r) ->
      let dctx, venv, prem, prems =
        analyze_if_eq_prem dctx exp.at exp.note optyp exp_l exp_r
      in
      let prem = prem $ at in
      (dctx, venv, prem, prems)
  | _ ->
      analyze_exp_as_bound dctx exp;
      let prem = IfPr exp $ at in
      (dctx, VEnv.empty, prem, [])

and analyze_let_prem (dctx : Dctx.t) (exp_l : exp) (binds_l : BEnv.t)
    (exp_r : exp) : Dctx.t * VEnv.t * prem' * prem list =
  let venv = BEnv.flatten binds_l in
  let dctx, renv_multi, exp_l =
    let renv_multi = Multibind.REnv.init binds_l in
    Multibind.rename_exp dctx renv_multi exp_l
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.REnv.gen_sideconditions binds_l renv_multi
  in
  let dctx, renv_partial, exp_l =
    Partialbind.rename_exp dctx (VEnv.dom venv) Partialbind.REnv.empty exp_l
  in
  let venv = update_venv_partial venv renv_partial in
  let sideconditions_partial =
    Partialbind.REnv.gen_sideconditions renv_partial
  in
  let dctx, renv_inject, exp_l =
    Injectbind.rename_exp dctx Injectbind.REnv.empty exp_l
  in
  let venv = update_venv_inject venv renv_inject in
  let prems_inject = Injectbind.REnv.gen_prems renv_inject in
  let prems = prems_inject @ sideconditions_multi @ sideconditions_partial in
  let prem = LetPr (exp_l, exp_r) in
  (dctx, venv, prem, prems)

and analyze_iter_prem (dctx : Dctx.t) (at : region) (prem : prem) (iter : iter)
    : Dctx.t * VEnv.t * prem * prem list =
  let dctx, venv, prem, prems = analyze_prem dctx prem in
  let venv = VEnv.map (Typ.add_iter iter) venv in
  let prems = List.map (fun prem -> IterPr (prem, (iter, [])) $ at) prems in
  let prem = IterPr (prem, (iter, [])) $ at in
  (dctx, venv, prem, prems)
