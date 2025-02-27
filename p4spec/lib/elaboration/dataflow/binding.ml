open Domain.Lib
open Il.Ast
open Attempt
module DCtx = Dctx
open Bind
open Envs
open Util.Source

let update_venv_multi (venv : VEnv.t) (renv_multi : Multibind.REnv.t) : VEnv.t =
  Multibind.REnv.fold
    (fun id ids_rename venv ->
      let ids_rename = IdSet.elements ids_rename in
      let dim = VEnv.find id venv in
      List.fold_left
        (fun venv id_rename -> VEnv.add id_rename dim venv)
        venv ids_rename)
    renv_multi venv

let update_venv_partial (venv : VEnv.t) (renv_partial : Partialbind.REnv.t) :
    VEnv.t =
  Partialbind.REnv.fold
    (fun id (_, dim) venv -> VEnv.add id dim venv)
    renv_partial venv

(* Expression binding analysis *)

let analyze_exps_as_bind (dctx : DCtx.t) (exps : exp list) :
    DCtx.t * VEnv.t * exp list * prem list =
  let binds = collect_exps dctx exps in
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
  let sideconditions = sideconditions_multi @ sideconditions_partial in
  (dctx, venv, exps, sideconditions)

let analyze_exp_as_bound (dctx : DCtx.t) (exp : exp) : unit =
  let binds = collect_exp dctx exp in
  if not (BEnv.is_empty binds) then
    error exp.at
      (Format.asprintf "expression has free variable(s): %s"
         (BEnv.to_string binds))

let analyze_exps_as_bound (dctx : DCtx.t) (exps : exp list) : unit =
  List.iter (analyze_exp_as_bound dctx) exps

(* Argument binding analysis *)

let analyze_args_as_bind (dctx : DCtx.t) (args : arg list) :
    DCtx.t * VEnv.t * arg list * prem list =
  let binds = collect_args dctx args in
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
  let sideconditions = sideconditions_multi @ sideconditions_partial in
  (dctx, venv, args, sideconditions)

(* Premise binding analysis *)

let rec analyze_prem (dctx : DCtx.t) (prem : prem) :
    DCtx.t * VEnv.t * prem * prem list =
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

and analyze_rule_prem (dctx : DCtx.t) (at : region) (id : id) (notexp : notexp)
    : DCtx.t * VEnv.t * prem * prem list =
  let mixop, exps = notexp in
  let hint = DCtx.find_hint dctx id in
  let exps_input, exps_output = Hint.Hint.split_exps hint exps in
  List.map snd exps_input |> analyze_exps_as_bound dctx;
  let dctx, venv, exps_output, sideconditions =
    let idxs, exps_output = List.split exps_output in
    let dctx, venv, exps_output, sideconditions =
      analyze_exps_as_bind dctx exps_output
    in
    let exps_output = List.combine idxs exps_output in
    (dctx, venv, exps_output, sideconditions)
  in
  let exps = Hint.Hint.combine_exps exps_input exps_output in
  let notexp = (mixop, exps) in
  let prem = RulePr (id, notexp) $ at in
  (dctx, venv, prem, sideconditions)

and analyze_if_eq_prem (dctx : DCtx.t) (at : region) (note : typ')
    (optyp : optyp) (exp_l : exp) (exp_r : exp) :
    DCtx.t * VEnv.t * prem' * prem list =
  let binds_l = collect_exp dctx exp_l in
  let binds_r = collect_exp dctx exp_r in
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

and analyze_if_prem (dctx : DCtx.t) (at : region) (exp : exp) :
    DCtx.t * VEnv.t * prem * prem list =
  match exp.it with
  | CmpE (`EqOp, optyp, exp_l, exp_r) ->
      let dctx, venv, prem, sideconditions =
        analyze_if_eq_prem dctx exp.at exp.note optyp exp_l exp_r
      in
      let prem = prem $ at in
      (dctx, venv, prem, sideconditions)
  | _ ->
      analyze_exp_as_bound dctx exp;
      let prem = IfPr exp $ at in
      (dctx, VEnv.empty, prem, [])

and analyze_let_prem (dctx : DCtx.t) (exp_l : exp) (binds_l : BEnv.t)
    (exp_r : exp) : DCtx.t * VEnv.t * prem' * prem list =
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
  let sideconditions = sideconditions_multi @ sideconditions_partial in
  let prem = LetPr (exp_l, exp_r) in
  (dctx, venv, prem, sideconditions)

and analyze_iter_prem (dctx : DCtx.t) (at : region) (prem : prem) (iter : iter)
    : DCtx.t * VEnv.t * prem * prem list =
  let dctx, venv, prem, sideconditions = analyze_prem dctx prem in
  let venv = VEnv.map (fun iters -> iters @ [ iter ]) venv in
  let sideconditions =
    List.map
      (fun sidecondition -> IterPr (sidecondition, (iter, [])) $ at)
      sideconditions
  in
  let prem = IterPr (prem, (iter, [])) $ at in
  (dctx, venv, prem, sideconditions)
