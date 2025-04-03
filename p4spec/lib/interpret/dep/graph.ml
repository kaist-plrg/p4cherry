open Il.Ast
open Util.Source

(* Ticker for node identifier tracking *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := id + 1;
  id

(* Value dependency graph

   Nodes is a mutable map from a value id to a node and its taint.
   Edges is a mutable map from a node to the set of nodes it depends on. *)

module G = Hashtbl.Make (struct
  type t = vid

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type t = { nodes : Node.t G.t; edges : Edges.t G.t }

(* Constructor *)

let empty () : t = { nodes = G.create 100000; edges = G.create 100000 }

(* Adders *)

let add_node ~(taint : bool) (graph : t ref) (value : value) : unit =
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
  let taint =
    let taints_from =
      List.map
        (fun vid_from -> vid_from |> G.find !graph.nodes |> Node.taint)
        vids_from
    in
    Node.init_taint ~init:taint taints_from
  in
  let node = (node, taint) in
  let edges =
    let edges = Edges.E.create (List.length vids_from) in
    List.iter
      (fun vid_from -> Edges.E.add edges (Edges.Inside, vid_from) ())
      vids_from;
    edges
  in
  G.add !graph.nodes vid node;
  G.add !graph.edges vid edges

let add_edge (graph : t ref) (value_from : value) (value_to : value)
    (label : Edges.label) : unit =
  let vid_from = value_from.note in
  let vid_to = value_to.note in
  (* Update the taint of the from node *)
  let mirror_from, taint_from = G.find !graph.nodes vid_from in
  let taint_to = G.find !graph.nodes vid_to |> Node.taint in
  let taint_from = Node.update_taint taint_from taint_to in
  let node_from = (mirror_from, taint_from) in
  G.add !graph.nodes vid_from node_from;
  (* Add an edge from the from node to the to node *)
  let edge = (label, vid_to) in
  let edges = G.find !graph.edges vid_from in
  Edges.E.add edges edge ()

(* Dot output *)

let dot_of_nodes (nodes : Node.t G.t) : string =
  G.fold
    (fun vid node dot ->
      Format.asprintf "%s\n%s" dot (Node.dot_of_node vid node))
    nodes ""

let dot_of_edges (edges : Edges.t G.t) : string =
  G.fold
    (fun vid_from edges dot ->
      Edges.E.fold
        (fun (label, vid_to) () dot ->
          Format.asprintf "%s\n  %s" dot
            (Edges.dot_of_edge vid_from label vid_to))
        edges dot)
    edges ""

let dot_of_graph (graph : t) : string =
  "digraph dependencies {\n" ^ dot_of_nodes graph.nodes
  ^ dot_of_edges graph.edges ^ "}"

(* Derivation *)

module S = Set.Make (Int)
module D = Map.Make (Int)

let useful (graph : t) (vids : S.t) : bool =
  S.exists
    (fun vid ->
      let taint = G.find graph.nodes vid |> Node.taint in
      taint = Node.Red || taint = Node.Pink)
    vids

let derive' (graph : t) (vid : vid) : S.t * int D.t =
  let vids_visited = ref (S.singleton vid) in
  let depths_visited = ref (D.singleton vid 0) in
  let vids_queue = Queue.create () in
  Queue.add (vid, 0) vids_queue;
  while not (Queue.is_empty vids_queue) do
    let vid_current, depth_current = Queue.take vids_queue in
    match G.find_opt graph.edges vid_current with
    | Some edges ->
        Edges.E.iter
          (fun (_, vid_from) () ->
            if not (S.mem vid_from !vids_visited) then (
              vids_visited := S.add vid_from !vids_visited;
              depths_visited :=
                D.add vid_from (depth_current + 1) !depths_visited;
              Queue.add (vid_from, depth_current + 1) vids_queue))
          edges
    | None -> ()
  done;
  (!vids_visited, !depths_visited)

let derive (graph : t) (prem : prem) (vid : vid) : unit =
  let vids_visited, depths_visited = derive' graph vid in
  if not (useful graph vids_visited) then ()
  else
    let oc = open_out (Format.asprintf "deps/%d.dot" vid) in
    "// " ^ string_of_region prem.at ^ "\n" |> output_string oc;
    "// " ^ Il.Print.string_of_prem prem ^ "\n" |> output_string oc;
    "// ranks where smaller the depth, more relevant it is\n"
    |> output_string oc;
    D.iter
      (fun vid depth ->
        let taint = G.find graph.nodes vid |> Node.taint in
        if taint = Node.Red then
          Format.asprintf "// [#%d]: %d\n" vid depth |> output_string oc)
      depths_visited;
    "digraph dependencies {\n" |> output_string oc;
    S.iter
      (fun vid ->
        let node = G.find graph.nodes vid in
        let interesting = node |> Node.taint |> Node.is_interesting in
        if interesting then
          let dot = Node.dot_of_node vid node in
          dot ^ "\n" |> output_string oc)
      vids_visited;
    S.iter
      (fun vid ->
        let from_interesting =
          G.find graph.nodes vid |> Node.taint |> Node.is_interesting
        in
        let edges = G.find graph.edges vid in
        Edges.E.iter
          (fun (label, vid_to) () ->
            let to_interesting =
              vid_to |> G.find graph.nodes |> Node.taint |> Node.is_interesting
            in
            if from_interesting && to_interesting && vid <> vid_to then
              let dot = Edges.dot_of_edge vid label vid_to in
              dot ^ "\n" |> output_string oc)
          edges)
      vids_visited;
    "}" |> output_string oc;
    close_out oc
