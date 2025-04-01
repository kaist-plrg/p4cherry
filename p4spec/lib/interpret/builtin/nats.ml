open Xl
open Il.Ast
module Value = Runtime_dynamic.Value
open Util.Source

(* Conversion between meta-numerics and OCaml numerics *)

let bigint_of_value (value : value) : Bigint.t =
  value |> Value.get_num |> Num.to_int

let value_of_bigint (ctx : Ctx.t) (i : Bigint.t) : value =
  let value = NumV (`Nat i) $$$ Dep.Node.fresh () in
  Ctx.add_node ctx value;
  value

(* dec $sum(nat* ) : nat *)

let sum (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  List.fold_left Bigint.( + ) Bigint.zero values |> value_of_bigint ctx

(* dec $max(nat* ) : nat *)

let max (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  List.fold_left Bigint.max Bigint.zero values |> value_of_bigint ctx

(* dec $min(nat* ) : nat *)

let min (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  List.fold_left Bigint.min Bigint.zero values |> value_of_bigint ctx
