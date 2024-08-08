open Syntax.Ast
open Util.Source
open Types
module Value = Runtime.Value

(* Context is consisted of layers of environments *)

type layer = Global | Block | Local
type frame = TDEnv.t * FDEnv.t * VEnv.t * TEnv.t

(* (TODO) Maybe refine the below type
   e.g., a local layer should never have an fdenv *)

type t = {
  cons : CDEnv.t;
  global : frame;
  block : id' * tparam' list * frame;
  local : id' * tparam' list * frame list;
}

let empty_frame = (TDEnv.empty, FDEnv.empty, VEnv.empty, TEnv.empty)

let empty =
  {
    cons = CDEnv.empty;
    global = empty_frame;
    block = ("", [], empty_frame);
    local = ("", [], []);
  }

(* Frame management *)

let enter_frame ctx =
  let id, tparams, frames = ctx.local in
  { ctx with local = (id, tparams, empty_frame :: frames) }

let exit_frame ctx =
  let id, tparams, frames = ctx.local in
  if frames = [] then Format.eprintf "(exit_frame) No frame to exit from\n";
  { ctx with local = (id, tparams, List.tl frames) }

(* Setters *)

let set_id layer id ctx =
  match layer with
  | Global ->
      Format.eprintf "(set_id) Global layer has no identifier\n";
      assert false
  | Block ->
      let _, tparams, frame = ctx.block in
      { ctx with block = (id, tparams, frame) }
  | Local ->
      let _, tparams, frames = ctx.local in
      { ctx with local = (id, tparams, frames) }

(* Getters *)

let get_id layer ctx =
  match layer with
  | Global -> ""
  | Block ->
      let id, _, _ = ctx.block in
      id
  | Local ->
      let id, _, _ = ctx.local in
      id

let rec get_tparams layer ctx =
  match layer with
  | Global -> []
  | Block ->
      let _, tparams, _ = ctx.block in
      tparams
  | Local ->
      let _, tparams, _ = ctx.local in
      tparams @ get_tparams Block ctx

(* Adders *)

let add_consdef cid cd ctx = { ctx with cons = CDEnv.add cid cd ctx.cons }

let add_tparam layer tparam ctx =
  match layer with
  | Global ->
      Format.eprintf "(add_tparam) Global layer cannot be type-parameterized\n";
      assert false
  | Block ->
      let id, tparams, frame = ctx.block in
      { ctx with block = (id, tparams @ [ tparam ], frame) }
  | Local ->
      let id, tparams, frames = ctx.local in
      { ctx with local = (id, tparams @ [ tparam ], frames) }

let add_typedef layer tid td ctx =
  match layer with
  | Global ->
      let tdenv, fdenv, venv, tenv = ctx.global in
      { ctx with global = (TDEnv.add tid td tdenv, fdenv, venv, tenv) }
  | Block ->
      let id_block, tparams, (tdenv, fdenv, venv, tenv) = ctx.block in
      {
        ctx with
        block = (id_block, tparams, (TDEnv.add tid td tdenv, fdenv, venv, tenv));
      }
  | Local ->
      let id_local, tparams, frames = ctx.local in
      let frame, frames =
        if frames = [] then (empty_frame, [])
        else (List.hd frames, List.tl frames)
      in
      let tdenv, fdenv, venv, tenv = frame in
      let frame = (TDEnv.add tid td tdenv, fdenv, venv, tenv) in
      { ctx with local = (id_local, tparams, frame :: frames) }

let add_funcdef layer fid fd ctx =
  match layer with
  | Global ->
      let tdenv, fdenv, venv, tenv = ctx.global in
      { ctx with global = (tdenv, FDEnv.add fid fd fdenv, venv, tenv) }
  | Block ->
      let id_block, tparams, (tdenv, fdenv, venv, tenv) = ctx.block in
      {
        ctx with
        block = (id_block, tparams, (tdenv, FDEnv.add fid fd fdenv, venv, tenv));
      }
  | Local ->
      let id_local, tparams, frames = ctx.local in
      let frame, frames = (List.hd frames, List.tl frames) in
      let tdenv, fdenv, venv, tenv = frame in
      let frame = (tdenv, FDEnv.add fid fd fdenv, venv, tenv) in
      { ctx with local = (id_local, tparams, frame :: frames) }

let add_value layer id value ctx =
  match layer with
  | Global ->
      let tdenv, fdenv, venv, tenv = ctx.global in
      { ctx with global = (tdenv, fdenv, VEnv.add id value venv, tenv) }
  | Block ->
      let id_block, tparams, (tdenv, fdenv, venv, tenv) = ctx.block in
      {
        ctx with
        block = (id_block, tparams, (tdenv, fdenv, VEnv.add id value venv, tenv));
      }
  | Local ->
      let id_local, tparams, frames = ctx.local in
      let frame, frames =
        if frames = [] then (empty_frame, [])
        else (List.hd frames, List.tl frames)
      in
      let tdenv, fdenv, venv, tenv = frame in
      let frame = (tdenv, fdenv, VEnv.add id value venv, tenv) in
      { ctx with local = (id_local, tparams, frame :: frames) }

let add_type layer id typ ctx =
  match layer with
  | Global ->
      let tdenv, fdenv, venv, tenv = ctx.global in
      { ctx with global = (tdenv, fdenv, venv, TEnv.add id typ tenv) }
  | Block ->
      let id_block, tparams, (tdenv, fdenv, venv, tenv) = ctx.block in
      {
        ctx with
        block = (id_block, tparams, (tdenv, fdenv, venv, TEnv.add id typ tenv));
      }
  | Local ->
      let id_local, tparams, frames = ctx.local in
      let frame, frames =
        if frames = [] then (empty_frame, [])
        else (List.hd frames, List.tl frames)
      in
      let tdenv, fdenv, venv, tenv = frame in
      let frame = (tdenv, fdenv, venv, TEnv.add id typ tenv) in
      { ctx with local = (id_local, tparams, frame :: frames) }

(* Finders *)

let find_cont finder layer id ctx = function
  | Some value -> Some value
  | None -> finder layer id ctx

let rec find_tparam_opt layer tparam ctx =
  match layer with
  | Global -> None
  | Block ->
      let _, tparams, _ = ctx.block in
      List.find_opt (fun tp -> tp = tparam) tparams
  | Local ->
      let _, tparams, _ = ctx.local in
      List.find_opt (fun tp -> tp = tparam) tparams
      |> find_cont find_tparam_opt Block tparam ctx

let find_tparam layer tparam ctx =
  find_tparam_opt layer tparam ctx |> Option.get

let rec find_typedef_opt layer tid ctx =
  match layer with
  | Global ->
      let tdenv, _, _, _ = ctx.global in
      TDEnv.find_opt tid tdenv
  | Block ->
      let _, _, (tdenv, _, _, _) = ctx.block in
      TDEnv.find_opt tid tdenv |> find_cont find_typedef_opt Global tid ctx
  | Local ->
      let _, _, frames = ctx.local in
      List.fold_left
        (fun td frame ->
          match td with
          | Some td -> Some td
          | None ->
              let tdenv, _, _, _ = frame in
              TDEnv.find_opt tid tdenv)
        None frames
      |> find_cont find_typedef_opt Block tid ctx

let find_typedef layer tid ctx = find_typedef_opt layer tid ctx |> Option.get

let rec find_funcdef_opt layer (fid, args) ctx =
  match layer with
  | Global ->
      let _, fdenv, _, _ = ctx.global in
      FDEnv.find_overloaded_opt (fid, args) fdenv
  | Block ->
      let _, _, (_, fdenv, _, _) = ctx.block in
      FDEnv.find_overloaded_opt (fid, args) fdenv
      |> find_cont find_funcdef_opt Global (fid, args) ctx
  | Local ->
      let _, _, frames = ctx.local in
      List.fold_left
        (fun fd frame ->
          match fd with
          | Some fd -> Some fd
          | None ->
              let _, fdenv, _, _ = frame in
              FDEnv.find_overloaded_opt (fid, args) fdenv)
        None frames
      |> find_cont find_funcdef_opt Block (fid, args) ctx

let find_funcdef layer (fid, args) ctx =
  find_funcdef_opt layer (fid, args) ctx |> Option.get

let rec find_value_opt layer id ctx =
  match layer with
  | Global ->
      let _, _, venv, _ = ctx.global in
      VEnv.find_opt id venv
  | Block ->
      let _, _, (_, _, venv, _) = ctx.block in
      VEnv.find_opt id venv |> find_cont find_value_opt Global id ctx
  | Local ->
      let _, _, frames = ctx.local in
      List.fold_left
        (fun value frame ->
          match value with
          | Some value -> Some value
          | None ->
              let _, _, venv, _ = frame in
              VEnv.find_opt id venv)
        None frames
      |> find_cont find_value_opt Block id ctx

let find_value layer id ctx = find_value_opt layer id ctx |> Option.get

let rec find_type_opt layer id ctx =
  match layer with
  | Global ->
      let _, _, _, tenv = ctx.global in
      TEnv.find_opt id tenv
  | Block ->
      let _, _, (_, _, _, tenv) = ctx.block in
      TEnv.find_opt id tenv |> find_cont find_type_opt Global id ctx
  | Local ->
      let _, _, frames = ctx.local in
      List.fold_left
        (fun typ frame ->
          match typ with
          | Some typ -> Some typ
          | None ->
              let _, _, _, tenv = frame in
              TEnv.find_opt id tenv)
        None frames
      |> find_cont find_type_opt Block id ctx

let find_type layer id ctx = find_type_opt layer id ctx |> Option.get

let find_opt finder_opt var ctx =
  match var.it with
  | Top id -> finder_opt Global id.it ctx
  | Bare id -> finder_opt Local id.it ctx

let find finder var ctx = find_opt finder var ctx |> Option.get

let find_overloaded_opt finder_overloaded_opt var args ctx =
  match var.it with
  | Top id -> finder_overloaded_opt Global (id.it, args) ctx
  | Bare id -> finder_overloaded_opt Local (id.it, args) ctx

let find_overloaded finder_overloaded var args ctx =
  find_overloaded_opt finder_overloaded var args ctx |> Option.get

(* Pretty-printer *)

let pp_frame fmt frame =
  let tdenv, fdenv, venv, tenv = frame in
  Format.fprintf fmt
    "@[@[<v 0>[Typedefs]:@ %a@]@\n\
     @[<v 0>[Functions]:@ %a@]@\n\
     @[<v 0>[Values]:@ %a@]@\n\
     @[<v 0>[Types]:@ %a@]@]" TDEnv.pp tdenv FDEnv.pp fdenv VEnv.pp venv TEnv.pp
    tenv

let pp fmt ctx =
  let id_block, tparams_block, frame_block = ctx.block in
  let id_local, tparams_local, frames_local = ctx.local in
  Format.fprintf fmt
    "@[@[<v 0>[[Constructors]]:@ %a@]@\n\
     @[<v 0>[[Global]]:@ %a@]@\n\
     @[<v 0>[[Block %s<%s>]]:@ %a@]@\n\
     @[<v 0>[[Local %s<%s>]]:@ %a@]@]" CDEnv.pp ctx.cons pp_frame ctx.global
    id_block
    (String.concat ", " tparams_block)
    pp_frame frame_block id_local
    (String.concat ", " tparams_local)
    (Format.pp_print_list pp_frame)
    frames_local
