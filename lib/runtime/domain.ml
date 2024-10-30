open Util.Source

(* Variable identifiers *)

module Id = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = compare
end

module IdSet = struct
  include Set.Make (Id)

  let pp fmt s =
    let pp_id fmt id = Format.fprintf fmt "%a" Id.pp id in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_id) (elements s)

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module IdMap = struct
  include Map.Make (Id)

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" Id.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let subset eq_v m1 m2 =
    List.for_all
      (fun (k, v1) ->
        match find_opt k m2 with Some v2 -> eq_v v1 v2 | None -> false)
      (bindings m1)

  let eq eq_v m1 m2 = subset eq_v m1 m2 && subset eq_v m2 m1
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Type identifiers *)

module TId = Id
module TIdSet = IdSet
module TIdMap = IdMap

(* Function identifiers *)

module FId = struct
  type t' = string
  type t = t' * string list

  let pp' fmt name = Format.fprintf fmt "%s" name
  let compare = compare

  let pp fmt (name, params) =
    Format.fprintf fmt "%s(%s)" name (String.concat ", " params)

  let to_fid (id : El.Ast.id) (params : El.Ast.param list) =
    let params =
      List.map
        (fun param ->
          let id, _, _, _, _ = param.it in
          id.it)
        params
    in
    (id.it, params)

  let to_names (args : El.Ast.arg list) =
    List.map
      (fun arg ->
        match arg.it with Lang.Ast.NameA (id, _) -> Some id.it | _ -> None)
      args
end

module FIdSet = struct
  include Set.Make (FId)

  let eq = equal

  let pp fmt s =
    let pp_fid fmt fid = Format.fprintf fmt "%a" FId.pp fid in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_fid) (elements s)

  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module FIdMap = struct
  include Map.Make (FId)

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" FId.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let subset eq_v m1 m2 =
    List.for_all
      (fun (k, v1) ->
        match find_opt k m2 with Some v2 -> eq_v v1 v2 | None -> false)
      (bindings m1)

  let eq eq_v m1 m2 = subset eq_v m1 m2 && subset eq_v m2 m1
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Constructor identifiers *)

module CId = FId
module CIdSet = FIdSet
module CIdMap = FIdMap

(* Environment functor *)

module MakeIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let pp fmt env = IdMap.pp V.pp fmt env

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" Id.pp id |> failwith
end

module MakeTIdEnv = MakeIdEnv

module MakeFIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include FIdMap

  type t = V.t FIdMap.t

  let pp fmt env = FIdMap.pp V.pp fmt env
  let find_opt (fid : FId.t) fenv = List.assoc_opt fid (bindings fenv)

  let find (fid : FId.t) fenv =
    match find_opt fid fenv with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" FId.pp fid |> failwith

  (* Precondition: args must be either all named or all unnamed *)
  let find_overloaded_opt (fid, args) fenv =
    let arity = List.length args in
    let args =
      if List.for_all Option.is_some args then
        List.map Option.get args |> List.sort String.compare
      else []
    in
    let func =
      List.find_opt
        (fun ((fid', params), _) ->
          let params = List.sort String.compare params in
          fid = fid'
          && arity = List.length params
          && (args = [] || List.for_all2 ( = ) args params))
        (bindings fenv)
    in
    Option.map snd func

  let find_overloaded (fid, args) fenv =
    match find_overloaded_opt (fid, args) fenv with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %s\n" fid |> failwith

  let find_non_overloaded_opt (fid : FId.t') fenv =
    let funcs =
      List.filter (fun ((fid', _), _) -> fid = fid') (bindings fenv)
    in
    assert (List.length funcs <= 1);
    match funcs with [] -> None | _ -> Some (List.hd funcs |> snd)

  let find_non_overloaded (fid : FId.t') fenv =
    match find_non_overloaded_opt fid fenv with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" FId.pp' fid |> failwith
end

module MakeCIdEnv = MakeFIdEnv

(* Pair functor *)

module MakePair (A : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (B : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = A.t * B.t

  let pp fmt (a, b) = Format.fprintf fmt "(%a, %a)" A.pp a B.pp b
end

(* Triple functor *)

module MakeTriple (A : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (B : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (C : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = A.t * B.t * C.t

  let pp fmt (a, b, c) = Format.fprintf fmt "(%a, %a, %a)" A.pp a B.pp b C.pp c
end
