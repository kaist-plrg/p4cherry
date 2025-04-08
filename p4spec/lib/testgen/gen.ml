module P4 = P4el.Ast
open Il.Ast
module Premkind = Runtime_testgen.Premkind
module Dep = Runtime_testgen.Dep
module Trace = Interpret.Trace
open Util.Source

(* Value dependency subgraph derivation *)

module S = Set.Make (Int)
module D = Map.Make (Int)

let useful (graph : Dep.Graph.t) (vids : S.t) : bool =
  S.exists
    (fun vid ->
      Dep.Graph.G.find graph.nodes vid
      |> Dep.Node.taint |> Dep.Node.is_interesting)
    vids

let derive_query' (graph : Dep.Graph.t) (vid : vid) : S.t * int D.t =
  let vids_visited = ref (S.singleton vid) in
  let depths_visited = ref (D.singleton vid 0) in
  let vids_queue = Queue.create () in
  Queue.add (vid, 0) vids_queue;
  while not (Queue.is_empty vids_queue) do
    let vid_current, depth_current = Queue.take vids_queue in
    match Dep.Graph.G.find_opt graph.edges vid_current with
    | Some edges ->
        Dep.Edges.E.iter
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

let derive_query (dirname : string) (graph : Dep.Graph.t) (prem : prem)
    (vid : vid) : unit =
  let vids_visited, depths_visited = derive_query' graph vid in
  if not (useful graph vids_visited) then ()
  else
    let oc = open_out (Format.asprintf "%s/%d.dot" dirname vid) in
    "// " ^ string_of_region prem.at ^ "\n" |> output_string oc;
    "// " ^ Il.Print.string_of_prem prem ^ "\n" |> output_string oc;
    "// ranks where smaller the depth, more relevant it is\n"
    |> output_string oc;
    D.iter
      (fun vid depth ->
        let taint = Dep.Graph.G.find graph.nodes vid |> Dep.Node.taint in
        if taint = Dep.Node.Red then
          Format.asprintf "// [#%d]: %d\n" vid depth |> output_string oc)
      depths_visited;
    "digraph dependencies {\n" |> output_string oc;
    S.iter
      (fun vid ->
        let node = Dep.Graph.G.find graph.nodes vid in
        let interesting = node |> Dep.Node.taint |> Dep.Node.is_interesting in
        if interesting then
          let dot = Dep.Node.dot_of_node vid node in
          dot ^ "\n" |> output_string oc)
      vids_visited;
    S.iter
      (fun vid ->
        let from_interesting =
          Dep.Graph.G.find graph.nodes vid
          |> Dep.Node.taint |> Dep.Node.is_interesting
        in
        let edges = Dep.Graph.G.find graph.edges vid in
        Dep.Edges.E.iter
          (fun (label, vid_to) () ->
            let to_interesting =
              vid_to
              |> Dep.Graph.G.find graph.nodes
              |> Dep.Node.taint |> Dep.Node.is_interesting
            in
            if from_interesting && to_interesting && vid <> vid_to then
              let dot = Dep.Edges.dot_of_edge vid label vid_to in
              dot ^ "\n" |> output_string oc)
          edges)
      vids_visited;
    "}" |> output_string oc;
    close_out oc

let derive (dirname : string) (trace : Trace.t) (graph : Dep.Graph.t) : unit =
  let queries = Trace.collect_queries trace in
  let oc = open_out (dirname ^ "/prems") in
  queries
  |> List.map (fun (prem, vid) ->
         let kind = prem |> Classify.classify_prem_bool |> Option.get in
         Format.asprintf "[#%d-%s] %s\n%s" vid (Premkind.to_string kind)
           (string_of_region prem.at)
           (Il.Print.string_of_prem prem))
  |> String.concat "\n\n" |> output_string oc;
  close_out oc;
  List.iter (fun (prem, vid) -> derive_query dirname graph prem vid) queries

let gen ~(dirname : string) (spec : spec) (program : P4.program) : unit =
  let trace, graph, _ =
    Interpret.Interp.run_typing ~debug:false ~profile:false ~derive:true spec
      program
  in
  let graph = Option.get graph in
  derive dirname trace graph
