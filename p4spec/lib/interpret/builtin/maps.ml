open Xl
open Il.Ast
module Value = Runtime_dynamic.Value
open Error
open Util.Source

(* Value map *)

module VMap = Map.Make (Value)

type map = Value.t VMap.t

(* Conversion between meta-maps and OCaml assoc lists *)

let map_of_value (value : value) : map =
  let tuple_of_value (value : value) : value * value =
    match value.it with
    | CaseV ([ [ { it = Atom "PAIR"; _ } ]; []; [] ], [ value_key; value_value ])
      ->
        (value_key, value_value)
    | _ ->
        error no_region
          (Format.asprintf "expected a pair, but got %s" (Value.to_string value))
  in
  match value.it with
  | CaseV ([ [ { it = Atom "MAP"; _ } ]; [] ], [ value_pairs ]) ->
      Value.get_list value_pairs |> List.map tuple_of_value
      |> List.fold_left
           (fun map (value_key, value_value) ->
             VMap.add value_key value_value map)
           VMap.empty
  | _ ->
      error no_region
        (Format.asprintf "expected a map, but got %s" (Value.to_string value))

let value_of_map (map : map) : value =
  let value_of_tuple ((value_key, value_value) : value * value) : value =
    CaseV
      ([ [ Atom.Atom "PAIR" $ no_region ]; []; [] ], [ value_key; value_value ])
    $$$ Ctx.note_plain ()
  in
  let value_pairs =
    ListV (VMap.bindings map |> List.map value_of_tuple) $$$ Ctx.note_plain ()
  in
  CaseV ([ [ Atom.Atom "MAP" $ no_region ]; [] ], [ value_pairs ])
  $$$ Ctx.note_plain ()

(* Built-in implementations *)

(* dec $find_map_opt<K, V>(map<K, V>, K) : V? *)

let find_map_opt (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_map, value_key = Extract.two at values_input in
  let map = map_of_value value_map in
  let value_opt = VMap.find_opt value_key map in
  match value_opt with
  | Some value -> OptV (Some value) $$$ Ctx.note_plain ()
  | None -> OptV None $$$ Ctx.note_plain ()

(* dec $find_maps_opt<K, V>(map<K, V>*, K) : V? *)

let find_maps_opt (at : region) (targs : targ list) (values_input : value list)
    : value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_maps, value_key = Extract.two at values_input in
  let maps = value_maps |> Value.get_list |> List.map map_of_value in
  let value_opt =
    List.fold_left
      (fun value_opt map ->
        match value_opt with
        | Some _ -> value_opt
        | None -> VMap.find_opt value_key map)
      None maps
  in
  match value_opt with
  | Some value -> OptV (Some value) $$$ Ctx.note_plain ()
  | None -> OptV None $$$ Ctx.note_plain ()

(* dec $add_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let add_map (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_map, value_key, value_value = Extract.three at values_input in
  map_of_value value_map |> VMap.add value_key value_value |> value_of_map

(* dec $adds_map<K, V>(map<K, V>, K*, V* ) : map<K, V> *)

let adds_map (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_map, value_keys, value_values = Extract.three at values_input in
  let map = map_of_value value_map in
  let values_key = value_keys |> Value.get_list in
  let values_value = value_values |> Value.get_list in
  List.fold_left2
    (fun map value_key value_value -> VMap.add value_key value_value map)
    map values_key values_value
  |> value_of_map

(* dec $update_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let update_map (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_map, value_key, value_value = Extract.three at values_input in
  map_of_value value_map |> VMap.add value_key value_value |> value_of_map
