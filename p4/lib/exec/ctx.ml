module L = Lang.Ast
module F = Format
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module Envs_static = Runtime_static.Envs
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module TEnv = Envs_dynamic.TEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
open Util.Pp
open Util.Source

(* Context is consisted of layers of environments *)

type cursor = Global | Block | Local

let pp_cursor fmt = function
  | Global -> F.pp_print_string fmt "Global"
  | Block -> F.pp_print_string fmt "Block"
  | Local -> F.pp_print_string fmt "Local"

(* Defining each layer *)

type gt = { cenv : CEnv.t; fenv : FEnv.t; venv : VEnv.t }
type bt = { tenv : TEnv.t; fenv : FEnv.t; venv : VEnv.t }
type lt = { tenv : TEnv.t; venvs : VEnv.t list }
type t = { global : gt; block : bt; local : lt }

let empty_gt = { cenv = CEnv.empty; fenv = FEnv.empty; venv = VEnv.empty }
let empty_bt = { tenv = TEnv.empty; fenv = FEnv.empty; venv = VEnv.empty }
let empty_lt = { tenv = TEnv.empty; venvs = [] }
let empty = { global = empty_gt; block = empty_bt; local = empty_lt }

(* Inheritance *)

let copy cursor ctx =
  match cursor with
  | Global -> { ctx with block = empty_bt; local = empty_lt }
  | Block -> { ctx with local = empty_lt }
  | Local -> ctx

(* Frame management *)

let enter_frame ctx =
  let venvs = VEnv.empty :: ctx.local.venvs in
  { ctx with local = { ctx.local with venvs } }

let exit_frame ctx =
  match ctx.local.venvs with
  | [] -> assert false
  | _ :: venvs -> { ctx with local = { ctx.local with venvs } }

(* Type resolution *)

let resolve_typ cursor typ ctx =
  match cursor with
  | Global -> typ
  | Block -> Type.subst ctx.block.tenv typ
  | Local ->
      let tenv = TEnv.extend ctx.block.tenv ctx.local.tenv in
      Type.subst tenv typ

(* Adders *)

let add_value cursor id value ctx =
  match cursor with
  | Global ->
      let venv = ctx.global.venv in
      let venv = VEnv.add_nodup id value venv in
      { ctx with global = { ctx.global with venv } }
  | Block ->
      let venv = ctx.block.venv in
      let venv = VEnv.add_nodup id value venv in
      { ctx with block = { ctx.block with venv } }
  | Local ->
      let venvs = ctx.local.venvs in
      let venv, venvs =
        if venvs = [] then (VEnv.empty, []) else (List.hd venvs, List.tl venvs)
      in
      let venv = VEnv.add_nodup id value venv in
      let venvs = venv :: venvs in
      { ctx with local = { ctx.local with venvs } }

let add_typ cursor id typ ctx =
  match cursor with
  | Global -> assert false
  | Block ->
      let tenv = ctx.block.tenv in
      let tenv = TEnv.add_nodup id typ tenv in
      { ctx with block = { ctx.block with tenv } }
  | Local ->
      let tenv = ctx.local.tenv in
      let tenv = TEnv.add_nodup id typ tenv in
      { ctx with local = { ctx.local with tenv } }

let add_typs cursor tparams targs ctx =
  List.fold_left
    (fun ctx (tparam, targ) -> add_typ cursor tparam.it targ.it ctx)
    ctx
    (List.combine tparams targs)

(* Updaters *)

let rec update_value_opt cursor id value ctx =
  match cursor with
  | Global ->
      VEnv.find_opt id ctx.global.venv
      |> Option.map (fun _ ->
             let venv = VEnv.add id value ctx.global.venv in
             { ctx with global = { ctx.global with venv } })
  | Block ->
      let ctx' =
        VEnv.find_opt id ctx.block.venv
        |> Option.map (fun _ ->
               let venv = VEnv.add id value ctx.block.venv in
               { ctx with block = { ctx.block with venv } })
      in
      if Option.is_some ctx' then ctx' else update_value_opt Global id value ctx
  | Local ->
      let venvs = ctx.local.venvs in
      let ctx', _ =
        List.fold_left
          (fun (ctx', venvs) venv ->
            match ctx' with
            | Some ctx' -> (Some ctx', venvs @ [ venv ])
            | None -> (
                match VEnv.find_opt id venv with
                | Some _ ->
                    let venv = VEnv.add id value venv in
                    let venvs = venvs @ [ venv ] in
                    let ctx = { ctx with local = { ctx.local with venvs } } in
                    (Some ctx, venvs)
                | None -> (None, venvs @ [ venv ])))
          (None, []) venvs
      in
      if Option.is_some ctx' then ctx' else update_value_opt Block id value ctx

let update_value cursor id value ctx =
  update_value_opt cursor id value ctx |> Option.get

(* Updater combinator *)

let update_opt updater_opt cursor var value ctx =
  match var.it with
  | L.Top id -> updater_opt Global id.it value ctx
  | L.Current id -> updater_opt cursor id.it value ctx

let update updater_opt cursor var value ctx =
  update_opt updater_opt cursor var value ctx |> Option.get

(* Finders *)

let find_cont finder cursor id ctx = function
  | Some value -> Some value
  | None -> finder cursor id ctx

(* Finder for value *)

let rec find_value_opt cursor id ctx =
  match cursor with
  | Global -> VEnv.find_opt id ctx.global.venv
  | Block ->
      VEnv.find_opt id ctx.block.venv |> find_cont find_value_opt Global id ctx
  | Local ->
      let venvs = ctx.local.venvs in
      List.fold_left
        (fun value venv ->
          match value with Some _ -> value | None -> VEnv.find_opt id venv)
        None venvs
      |> find_cont find_value_opt Block id ctx

let find_value cursor id ctx = find_value_opt cursor id ctx |> Option.get

(* Finder for type *)

let rec find_typ_opt cursor id ctx =
  match cursor with
  | Global -> None
  | Block ->
      TEnv.find_opt id ctx.block.tenv |> find_cont find_typ_opt Global id ctx
  | Local ->
      let tenv = ctx.local.tenv in
      TEnv.find_opt id tenv |> find_cont find_typ_opt Block id ctx

let find_typ cursor id ctx = find_typ_opt cursor id ctx |> Option.get

(* Finder for function *)

let rec find_func_at_opt cursor (fname, args) ctx =
  match cursor with
  | Global ->
      FEnv.find_func_opt (fname, args) ctx.global.fenv
      |> Option.map (fun func -> (func, Global))
  | Block ->
      FEnv.find_func_opt (fname, args) ctx.block.fenv
      |> Option.map (fun func -> (func, Block))
      |> find_cont find_func_at_opt Global (fname, args) ctx
  | Local -> find_func_at_opt Block (fname, args) ctx

let find_func_at cursor (fname, args) ctx =
  find_func_at_opt cursor (fname, args) ctx |> Option.get

let find_func_opt cursor (fname, args) ctx =
  find_func_at_opt cursor (fname, args) ctx |> Option.map fst

let find_func cursor (fname, args) ctx =
  find_func_opt cursor (fname, args) ctx |> Option.get

(* Finder combinator *)

let find_opt finder_opt cursor var ctx =
  match var.it with
  | L.Top id -> finder_opt Global id.it ctx
  | L.Current id -> finder_opt cursor id.it ctx

let find finder_opt cursor var ctx =
  find_opt finder_opt cursor var ctx |> Option.get

let find_f_opt finder_f_opt cursor var args ctx =
  match var.it with
  | L.Top id -> finder_f_opt Global (id.it, args) ctx
  | L.Current id -> finder_f_opt cursor (id.it, args) ctx

let find_f finder_f_opt cursor var args ctx =
  find_f_opt finder_f_opt cursor var args ctx |> Option.get

let find_f_at_opt finder_f_at_opt cursor var args ctx =
  match var.it with
  | L.Top id -> finder_f_at_opt Global (id.it, args) ctx
  | L.Current id -> finder_f_at_opt cursor (id.it, args) ctx

let find_f_at finder_f_at_opt cursor var args ctx =
  find_f_at_opt finder_f_at_opt cursor var args ctx |> Option.get

(* Pretty-printer *)

let pp_gt ?(level = 0) fmt (gt : gt) =
  F.fprintf fmt "%s[[Global Layer]]\n%s[Functions]%a\n%s[Values]%a\n"
    (indent level)
    (indent (level + 1))
    (FEnv.pp ~level:(level + 2))
    gt.fenv
    (indent (level + 1))
    (VEnv.pp ~level:(level + 2))
    gt.venv

let pp_bt ?(level = 0) fmt (bt : bt) =
  F.fprintf fmt
    "%s[[Block Layer]]\n%s[Types]%a\n%s[Functions]%a\n%s[Values]%a\n"
    (indent level)
    (indent (level + 1))
    (TEnv.pp ~level:(level + 2))
    bt.tenv
    (indent (level + 1))
    (FEnv.pp ~level:(level + 2))
    bt.fenv
    (indent (level + 1))
    (VEnv.pp ~level:(level + 2))
    bt.venv

let pp_lt ?(level = 0) fmt (lt : lt) =
  F.fprintf fmt "%s[[Local Layer]]\n%s[Types]%a\n%s[Values]\n%a\n"
    (indent level)
    (indent (level + 1))
    (TEnv.pp ~level:(level + 2))
    lt.tenv
    (indent (level + 1))
    (pp_list ~level:(level + 2) VEnv.pp ~sep:Nl)
    lt.venvs

let pp fmt ctx =
  F.fprintf fmt "===== Context =====\n%a%a%a" (pp_gt ~level:0) ctx.global
    (pp_bt ~level:0) ctx.block (pp_lt ~level:0) ctx.local
