module CS = C.Syntax

(* Note: action data does not include parameters included from lambda lifting *)
type action_signature = {
  action_name : string;
  action_data : CS.cparam list;
  compiled_function_name : string;
}

type t = { params : CS.cparam list; action_signatures : action_signature list }

let empty : t = { params = []; action_signatures = [] }

let mem_param (ctx : t) (id : string) : bool =
  List.exists (fun (_, cvar) -> cvar = id) ctx.params

let add_param (ctx : t) (param : CS.cparam) : t =
  if mem_param ctx (snd param) then ctx
  else { ctx with params = param :: ctx.params }

let add_params (ctx : t) (params : CS.cparam list) : t =
  List.fold_left (fun ctx param -> add_param ctx param) ctx params

let get_params (ctx : t) : CS.cparam list = List.rev ctx.params

let mem_action_signature (ctx : t) (action_name : string) : bool =
  List.exists
    (fun { action_name = name; _ } -> name = action_name)
    ctx.action_signatures

let add_action_signature (ctx : t) (action_signature : action_signature) : t =
  if mem_action_signature ctx action_signature.action_name then ctx
  else
    { ctx with action_signatures = action_signature :: ctx.action_signatures }

let add_action_signatures (ctx : t) (action_signatures : action_signature list)
    : t =
  List.fold_left
    (fun ctx action_signature -> add_action_signature ctx action_signature)
    ctx action_signatures

let get_action_signatures (ctx : t) : action_signature list =
  ctx.action_signatures

let get_action_signature (ctx : t) (action_name : string) :
    action_signature option =
  List.find_opt
    (fun { action_name = name; _ } -> name = action_name)
    ctx.action_signatures

(* Note: I don't have to maintain a symbol table from symbols to types.
   The only case where I need scope so far is when I need to know the type of a symbol
   is during member field access when I need to know whether I should dereference
   the struct or not before accessing the field. P4 doesn't support pointers.
   The only pointers in the program will be the C function parameters.
   Therefore it is enough if I simply maintain the list of function parameters in scope.

   Furthermore, I can assume that the program has been typechecked and so I don't have to check stuff again *)
