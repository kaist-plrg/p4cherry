open Il.Ast
module Premkind = Runtime_testgen.Premkind
open Util.Source

(* Classification of if-premises *)

module C = Map.Make (Premkind)

let classify_exp_bool (exp : exp) : Premkind.t option =
  match exp.it with
  | BoolE _ -> Some BoolK
  | VarE _ -> Some VarK
  | UnE (unop, _, _) -> (
      match unop with `NotOp -> Some (UnK `Not) | _ -> None)
  | BinE (binop, _, _, _) -> (
      match binop with
      | `AndOp -> Some (BinK `And)
      | `OrOp -> Some (BinK `Or)
      | `ImplOp -> Some (BinK `Impl)
      | `EquivOp -> Some (BinK `Equiv)
      | _ -> None)
  | CmpE (cmpop, _, _, _) -> (
      match cmpop with
      | `EqOp -> Some (CmpK `Eq)
      | `NeOp -> Some (CmpK `Ne)
      | `LtOp -> Some (CmpK `Lt)
      | `GtOp -> Some (CmpK `Gt)
      | `LeOp -> Some (CmpK `Le)
      | `GeOp -> Some (CmpK `Ge))
  | SubE _ -> Some SubK
  | MatchE _ -> Some MatchK
  | MemE _ -> Some MemK
  | DotE _ -> Some DotK
  | IdxE _ -> Some IdxK
  | CallE _ -> Some CallK
  | _ -> None

let rec classify_prem_bool (prem : prem) : Premkind.t option =
  match prem.it with
  | IfPr exp -> classify_exp_bool exp
  | IterPr (prem, _) -> classify_prem_bool prem
  | _ -> None

let classify_rule (rule : rule) : (Premkind.t * prem) list =
  let _, _, prems = rule.it in
  List.filter_map
    (fun prem ->
      match classify_prem_bool prem with
      | Some kind -> Some (kind, prem)
      | None -> None)
    prems

let classify_clause (clause : clause) : (Premkind.t * prem) list =
  let _, _, prems = clause.it in
  List.filter_map
    (fun prem ->
      match classify_prem_bool prem with
      | Some kind -> Some (kind, prem)
      | None -> None)
    prems

let classify_def (def : def) : (Premkind.t * prem) list =
  match def.it with
  | TypD _ -> []
  | RelD (_, _, _, rules) -> List.concat_map classify_rule rules
  | DecD (_, _, _, _, clauses) -> List.concat_map classify_clause clauses

let classify_spec ~(filename : string) (spec : spec) : unit =
  let prems_group =
    spec
    |> List.concat_map classify_def
    |> List.fold_left
         (fun prems_group (kind, prem) ->
           match C.find_opt kind prems_group with
           | Some prems -> C.add kind (prems @ [ prem ]) prems_group
           | None -> C.add kind [ prem ] prems_group)
         C.empty
  in
  C.iter
    (fun kind prems ->
      let oc =
        open_out
          (Format.asprintf "%s-%s.prem" filename (Premkind.to_string kind))
      in
      List.iteri
        (fun idx prem ->
          Format.asprintf "(* %d *)\n;; %s\n%s\n\n" idx
            (string_of_region prem.at)
            (Il.Print.string_of_prem prem)
          |> output_string oc)
        prems;
      close_out oc)
    prems_group
