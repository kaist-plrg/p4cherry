open Il.Ast
module Value = Runtime_dynamic.Value
open Util.Source

(* dec $rev_<X>(X* ) : X* *)

let rev_ (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let value = ListV (List.rev values) $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value;
  value

(* dec $concat_<X>((X* )* ) : X* *)

let concat_ (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values =
    Extract.one at values_input
    |> Value.get_list
    |> List.concat_map Value.get_list
  in
  let value = ListV values $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value;
  value

(* dec $distinct_<K>(K* ) : bool *)

let distinct_ (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let set = Sets.VSet.of_list values in
  let value =
    BoolV (Sets.VSet.cardinal set = List.length values) $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ctx value;
  value
