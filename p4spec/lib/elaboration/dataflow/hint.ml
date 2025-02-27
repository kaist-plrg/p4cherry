open Domain.Lib
open Il.Ast

(* Input hints for rules *)

module Hint = struct
  type t = int list

  let to_string t =
    Format.asprintf "hint(input %s)"
      (String.concat " " (List.map (fun idx -> "%" ^ string_of_int idx) t))

  let split_exps (hint : t) (exps : exp list) :
      (int * exp) list * (int * exp) list =
    exps
    |> List.mapi (fun idx exp -> (idx, exp))
    |> List.partition (fun (idx, _) -> List.mem idx hint)

  let combine_exps (exps_input : (int * exp) list)
      (exps_output : (int * exp) list) : exp list =
    exps_input @ exps_output
    |> List.sort (fun (idx_i, _) (idx_o, _) -> compare idx_i idx_o)
    |> List.map snd
end

(* Environment for input hints *)

module HEnv = MakeIdEnv (Hint)
