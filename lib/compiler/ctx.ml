module CS = C.Syntax

type t = { params : CS.cparam list }

let empty : t = { params = [] }

let mem_param (ctx : t) (id : string) : bool =
  List.exists (fun (_, cvar) -> cvar = id) ctx.params

let add_param (ctx : t) (param : CS.cparam) : t =
  if mem_param ctx (snd param) then ctx else { params = param :: ctx.params }

let add_params (ctx : t) (params : CS.cparam list) : t =
  List.fold_left (fun ctx param -> add_param ctx param) ctx params

let get_params (ctx : t) : CS.cparam list = ctx.params

(* Note: I don't have to maintain a symbol table from symbols to types.
   The only case where I need scope so far is when I need to know the type of a symbol
   is during member field access when I need to know whether I should dereference
   the struct or not before accessing the field. P4 doesn't support pointers.
   The only pointers in the program will be the C function parameters.
   Therefore it is enough if I simply maintain the list of function parameters in scope.

   Furthermore, I can assume that the program has been typechecked and so I don't have to check stuff again *)
