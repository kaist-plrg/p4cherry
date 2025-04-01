open Xl
open Il.Ast

(* Ticker for node identifier tracking *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := id + 1;
  id

(* Nodes are a mirror of the runtime values *)

type t' =
  | EmptyN
  | BoolN of bool
  | NumN of Num.t
  | TextN of string
  | StructN of (atom * vid) list
  | CaseN of mixop * vid list
  | TupleN of vid list
  | OptN of vid option
  | ListN of vid list
  | FuncN of id

type t = Pend of vid * t' | Commit of vid * t'

let vid = function Pend (vid, _) -> vid | Commit (vid, _) -> vid

(* Hashing *)

let equal node_a node_b = vid node_a = vid node_b
let hash node = Hashtbl.hash (vid node)
