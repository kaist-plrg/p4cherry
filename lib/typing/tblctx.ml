module Types = Runtime.Types
module Type = Types.Type
open Util.Source

type pi = {
  priorities : int list;
  priority_delta : int;
  priority_init : bool;
  largest_priority_wins : bool;
}

type ei = {
  size : int;
  prefix : int;
  max_prefix : int;
  const : bool;
}

type state = NoPri | NoPriLpm | Pri | PriLpm

type t = {
  keys : (Type.t * Il.Ast.match_kind') list;
  actions : (Il.Ast.var' * Types.param list * Il.Ast.arg list) list;
  priorities_info : pi;
  entries_info : ei;
  state : state;
}

let empty_pi =
  {
    priorities = [];
    priority_delta = 1;
    priority_init = false;
    largest_priority_wins = true;
  }

let empty_ei = { size = 0; prefix = 0; max_prefix = 0; const = true }

let empty =
  {
    keys = [];
    actions = [];
    priorities_info = empty_pi;
    entries_info = empty_ei;
    state = NoPri;
  }

let add_key table_key table_ctx =
  { table_ctx with keys = table_ctx.keys @ [ table_key ] }

let add_action table_action table_ctx =
  { table_ctx with actions = table_ctx.actions @ [ table_action ] }

let set_largest_priority_wins largest_priority_wins table_ctx =
  {
    table_ctx with
    priorities_info = { table_ctx.priorities_info with largest_priority_wins };
  }

let set_priority_delta priority_delta table_ctx =
  {
    table_ctx with
    priorities_info = { table_ctx.priorities_info with priority_delta };
  }

let set_state state table_ctx = { table_ctx with state }

let set_const_entries const table_ctx =
  { table_ctx with entries_info = { table_ctx.entries_info with const } }

let set_priority_init priority_init table_ctx =
  if List.length table_ctx.priorities_info.priorities == 0 then
    {
      table_ctx with
      priorities_info = { table_ctx.priorities_info with priority_init };
    }
  else table_ctx

let add_priority priority table_ctx =
  {
    table_ctx with
    priorities_info =
      {
        table_ctx.priorities_info with
        priorities = table_ctx.priorities_info.priorities @ [ priority ];
      };
  }

let set_entries_size size table_ctx =
  { table_ctx with entries_info = { table_ctx.entries_info with size } }

let set_prefix prefix table_ctx =
  { table_ctx with entries_info = { table_ctx.entries_info with prefix } }

let set_max_prefix max_prefix table_ctx =
  { table_ctx with entries_info = { table_ctx.entries_info with max_prefix } }

let set_prefix_to_max table_ctx =
  { table_ctx with entries_info = { table_ctx.entries_info with prefix = table_ctx.entries_info.max_prefix} }

let get_last_priority table_ctx =
  let len = List.length table_ctx.priorities_info.priorities - 1 in
  List.nth table_ctx.priorities_info.priorities len

let is_priorities_empty table_ctx =
  List.length table_ctx.priorities_info.priorities == 0

let is_mem_priorities priority table_ctx =
  List.mem priority table_ctx.priorities_info.priorities

let find_action table_ctx var =
  List.fold_left
    (fun table_action (var_action, params_action, args_action) ->
      match table_action with
      | Some _ -> table_action
      | None ->
          if Il.Eq.eq_var' var.it var_action then
            Some (params_action, args_action)
          else table_action)
    None table_ctx.actions
