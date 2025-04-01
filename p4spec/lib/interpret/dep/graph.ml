open Il.Ast
open Util.Source

(* Value dependency graph

   A mutable map from a node to the set of nodes it depends on. *)

module G = Hashtbl.Make (Node)

type t = Edges.t G.t

(* Constructor *)

let empty () : t = G.create 100_000

(* Adders *)

let add_node (graph : t) (value : value) : unit =
  let vid = value.note in
  let node, vids_from =
    match value.it with
    | BoolV b -> (Node.BoolN b, [])
    | NumV n -> (Node.NumN n, [])
    | TextV s -> (Node.TextN s, [])
    | StructV valuefields ->
        let atoms, values = List.split valuefields in
        let vids_from = List.map (fun value -> value.note) values in
        let vidfields = List.combine atoms vids_from in
        (Node.StructN vidfields, vids_from)
    | CaseV (mixop, values) ->
        let vids_from = List.map note values in
        (Node.CaseN (mixop, vids_from), vids_from)
    | TupleV values ->
        let vids_from = List.map note values in
        (Node.TupleN vids_from, vids_from)
    | OptV None -> (Node.OptN None, [])
    | OptV (Some value) ->
        let vid_from = note value in
        (Node.OptN (Some vid_from), [ vid_from ])
    | ListV values ->
        let vids_from = List.map note values in
        (Node.ListN vids_from, vids_from)
    | FuncV id -> (Node.FuncN id, [])
  in
  let node = Node.Pend (vid, node) in
  let edges =
    let edges = Edges.E.create (List.length vids_from) in
    List.iter
      (fun vid_from -> Edges.E.add edges (Edges.Inside, vid_from) ())
      vids_from;
    edges
  in
  G.add graph node edges

let add_edge (graph : t) (value_from : value) (value_to : value)
    (label : Edges.label) : unit =
  let vid_from = value_from.note in
  let vid_to = value_to.note in
  let edge = (label, vid_to) in
  let node_from = Node.Pend (vid_from, Node.EmptyN) in
  match G.find_opt graph node_from with
  | Some edges -> Edges.E.add edges edge ()
  | None ->
      let edges = Edges.E.create 1 in
      Edges.E.add edges edge ();
      G.add graph node_from edges
