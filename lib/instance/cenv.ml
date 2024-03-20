open Syntax
open Ast
open Utils

(* Constructor closure environment *)

type t = Cclosure.t Var.VMap.t list

let empty = [ Var.VMap.empty ]

let current (cenv : t) =
  assert (List.length cenv > 0);
  (List.hd cenv, List.tl cenv)

let enter (cenv : t) = Var.VMap.empty :: cenv

let exit (cenv : t) =
  assert (List.length cenv > 0);
  List.tl cenv

let insert (var : Var.t) (cclos : Cclosure.t) (cenv : t) =
  let now, old = current cenv in
  let now = Var.VMap.add var cclos now in
  now :: old

let find (var : Var.t) (cenv : t) =
  let rec find' cenv =
    match cenv with
    | [] -> None
    | now :: old -> (
        match Var.VMap.find_opt var now with
        | Some value -> Some value
        | None -> find' old)
  in
  match find' cenv with
  | Some cclos -> cclos
  | None -> Printf.sprintf "Constructor closure %s not found" (Var.print var) |> failwith

let find_toplevel (var : Var.t) (cenv : t) =
  let top = List.rev cenv |> List.hd in
  match Var.VMap.find_opt var top with
  | Some value -> value
  | None -> Printf.sprintf "Constructor closure %s not found in top scope" (Var.print var) |> failwith

let rec find_from_type (typ : Type.t) (cenv : t) =
  match typ with
  | Type.TypeName { name = Name.BareName text; _ } ->
      find text.str cenv
  | Type.TypeName { name = Name.QualifiedName ([], text); _ } ->
      find_toplevel text.str cenv
  (* (TODO) how to consider the type arguments? *)
  | Type.SpecializedType { base; _ } ->
      find_from_type base cenv
  | _ ->
    Printf.sprintf
      "Constructor closure %s not found" (Pretty.print_type typ) |> failwith

let print ?(indent = 0) (cenv : t) =
  let print_binding var cclos acc =
    acc @ [ Printf.sprintf "%s: %s" (Var.print var) (Cclosure.print cclos) ]
  in
  let print' acc cenv =
    let bindings = Var.VMap.fold print_binding cenv [] in
    let bindings = String.concat ", " bindings in
    acc @ [ Printf.sprintf "%s[ %s ]" (Print.print_indent indent) bindings ]
  in
  let scenv = List.fold_left print' [] cenv in
  String.concat "\n" scenv
