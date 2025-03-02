module F = Format
open Util.Pp
open Util.Source
open Util.Error

let check = check_checker
let error_no_info = error_checker_no_info

(* Variable identifiers *)

module Id = struct
  type t = string

  let pp fmt t = F.fprintf fmt "%s" t
  let compare = compare
end

module IdSet = struct
  include Set.Make (Id)

  let pp fmt s =
    let pp_id fmt id = F.fprintf fmt "%a" Id.pp id in
    F.fprintf fmt "{ %a }" (pp_list pp_id ~sep:Comma) (elements s)

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module IdMap = struct
  include Map.Make (Id)

  type 'v pp_v = ?level:int -> F.formatter -> 'v -> unit

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp ?(level = 0) (pp_v : 'v pp_v) fmt m =
    let pp_binding fmt (k, v) =
      F.fprintf fmt "%a : %a" Id.pp k (pp_v ~level:(level + 2)) v
    in
    let bindings = bindings m in
    F.fprintf fmt "{\n%a\n%s}"
      (pp_list ~level:(level + 1) pp_binding ~sep:Nl)
      bindings (indent level)

  let extend env_a env_b =
    List.fold_left (fun env (k, v) -> add k v env) env_a (bindings env_b)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Type identifiers *)

module TId = Id
module TIdSet = IdSet
module TIdMap = IdMap

(* Function identifiers *)

module FId = struct
  type t = name * param list
  and name = string
  and param = string * bool

  let pp_name fmt name = F.fprintf fmt "%s" name
  let compare = compare
  let pp_param fmt (id, _) = F.fprintf fmt "%s" id

  let pp fmt (name, params) =
    F.fprintf fmt "%a(%a)" pp_name name (pp_list pp_param ~sep:Comma) params

  let to_fid id params =
    let params =
      List.map
        (fun param ->
          let id, _, _, value_default, _ = param.it in
          (id.it, Option.is_some value_default))
        params
    in
    (id.it, params)

  let to_names args =
    List.map
      (fun arg ->
        match arg.it with Lang.Ast.NameA (id, _) -> Some id.it | _ -> None)
      args
end

module FIdSet = struct
  include Set.Make (FId)

  let eq = equal

  let pp fmt s =
    let pp_fid fmt fid = F.fprintf fmt "%a" FId.pp fid in
    F.fprintf fmt "{ %a }" (pp_list pp_fid ~sep:Comma) (elements s)

  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module FIdMap = struct
  include Map.Make (FId)

  type 'v pp_v = ?level:int -> F.formatter -> 'v -> unit

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp ?(level = 0) (pp_v : 'v pp_v) fmt m =
    let pp_binding fmt (k, v) =
      F.fprintf fmt "%a : %a" FId.pp k (pp_v ~level:(level + 2)) v
    in
    let bindings = bindings m in
    F.fprintf fmt "{\n%a\n%s}"
      (pp_list ~level:(level + 1) pp_binding ~sep:Nl)
      bindings (indent level)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Constructor identifiers *)

module CId = FId
module CIdSet = FIdSet
module CIdMap = FIdMap

(* Object identifiers *)

module OId = struct
  type t = Id.t list

  let pp fmt t = String.concat "." t |> F.fprintf fmt "%s"
  let compare = compare
end

module OIdSet = struct
  include Set.Make (OId)

  let pp fmt s =
    let pp_oid fmt oid = F.fprintf fmt "%a" OId.pp oid in
    F.fprintf fmt "{ %a }" (pp_list pp_oid ~sep:Comma) (elements s)

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module OIdMap = struct
  include Map.Make (OId)

  type 'v pp_v = ?level:int -> F.formatter -> 'v -> unit

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp ?(level = 0) (pp_v : 'v pp_v) fmt m =
    let pp_binding fmt (k, v) =
      F.fprintf fmt "%a : %a" OId.pp k (pp_v ~level:(level + 2)) v
    in
    let bindings = bindings m in
    F.fprintf fmt "{\n%a\n%s}"
      (pp_list ~level:(level + 1) pp_binding ~sep:Nl)
      bindings (indent level)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Environment functor *)

module MakeIdEnv (V : sig
  type t

  val pp : ?level:int -> F.formatter -> t -> unit
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let pp ?(level = 0) fmt env = IdMap.pp ~level V.pp fmt env

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> F.asprintf "key not found: %a" Id.pp id |> error_no_info

  let add_nodup id value env =
    if mem id env then
      F.asprintf "key already exists: %a" Id.pp id |> error_no_info
    else add id value env

  let extend_nodup env_a env_b =
    List.fold_left (fun env (k, v) -> add_nodup k v env) env_a (bindings env_b)
end

module MakeTIdEnv = MakeIdEnv

module MakeFIdEnv (V : sig
  type t

  val pp : ?level:int -> F.formatter -> t -> unit
  val eq_kind : t -> t -> bool
end) =
struct
  include FIdMap

  type t = V.t FIdMap.t

  let pp ?(level = 0) fmt env = FIdMap.pp ~level V.pp fmt env

  (* Lookup for matching def site to def site *)
  (* (TODO) This must also impose the restriction that default values must be coherent, if specified *)

  let find_opt (fid : FId.t) fenv = List.assoc_opt fid (bindings fenv)

  let find (fid : FId.t) fenv =
    match find_opt fid fenv with
    | Some func -> func
    | None -> F.asprintf "key not found: %a" FId.pp fid |> error_no_info

  (* Lookups for matching call site to def site *)

  let check_named_args args =
    check
      (List.for_all Option.is_some args || List.for_all Option.is_none args)
      "(check_named_args) either all or no arguments must specify the \
       parameter name"

  (* (6.8.1) Justification

     Following is a summary of the constraints imposed by the parameter directions:

      - If parameters with default values do not appear at the end of the list of parameters,
        invocations that use the default values must use named arguments. *)

  (* (8.20) Method invocations and function calls

     A function call or method invocation can optionally specify for each argument
     the corresponding parameter name.
     It is illegal to use names only for some arguments:
     either all or no arguments must specify the parameter name. *)

  let check_func_name fname fname' = fname = fname'
  let check_arity_more args params = List.length args > List.length params
  let check_arity args params = List.length args = List.length params
  let check_args_named arg_names = arg_names <> []

  let find_match_named fid func arg_names params =
    let param_names = List.map fst params in
    let param_names = List.sort String.compare param_names in
    if List.for_all2 ( = ) arg_names param_names then Some (fid, func, [])
    else None

  let find_match_named_default fid func arg_names params =
    let param_names = List.map fst params in
    let param_names = List.sort String.compare param_names in
    let param_missing_names =
      List.filter
        (fun param_name -> not (List.mem param_name arg_names))
        param_names
    in
    let arg_names =
      arg_names @ param_missing_names |> List.sort String.compare
    in
    find_match_named fid func arg_names params
    |> Option.map (fun (fid, func, _) -> (fid, func, param_missing_names))

  let find_match_unnamed_default fid func args params =
    let params_default =
      List.filteri (fun i _ -> i >= List.length args) params
    in
    let params_names, params_default = List.split params_default in
    if List.for_all Fun.id params_default then Some (fid, func, params_names)
    else None

  let find_func_opt (fname, args) fenv =
    check_named_args args;
    let arg_names =
      if List.for_all Option.is_some args then
        List.map Option.get args |> List.sort String.compare
      else []
    in
    let funcs =
      List.filter_map
        (fun (fid, func) ->
          let fname', params = fid in
          (* Falls into roughly five cases:
             (1) Name mismatch or more args than params
             (2) Arity match
                (a) Named args (b) Unnamed args
             (3) Arity mismatch (maybe due to default param)
                (a) Named args (b) Unnamed args *)
          if not (check_func_name fname fname') then None
          else if check_arity_more args params then None
          else if check_arity args params then
            if check_args_named arg_names then
              find_match_named fid func arg_names params
            else Some (fid, func, [])
          else if check_args_named arg_names then
            find_match_named_default fid func arg_names params
          else find_match_unnamed_default fid func args params)
        (bindings fenv)
    in
    match funcs with
    | [] -> None
    | [ func ] -> Some func
    | _ ->
        F.asprintf
          "(find_overloaded_opt) cannot resolve overloaded function given %a"
          FId.pp_name fname
        |> error_no_info

  let find_func (fname, args) fenv =
    match find_func_opt (fname, args) fenv with
    | Some value -> value
    | _ -> F.asprintf "key not found: %a" FId.pp_name fname |> error_no_info

  let find_funcs_by_name fname fenv =
    List.filter (fun ((fname', _), _) -> fname = fname') (bindings fenv)
    |> List.map snd

  let find_func_by_name_opt fname fenv =
    let funcs = find_funcs_by_name fname fenv in
    match funcs with
    | [] -> None
    | [ func ] -> Some func
    | _ ->
        F.asprintf
          "(find_func_by_name_opt) cannot resolve overloaded function given %a"
          FId.pp_name fname
        |> error_no_info

  let find_func_by_name fname fenv =
    match find_func_by_name_opt fname fenv with
    | Some value -> value
    | _ -> F.asprintf "key not found: %a" FId.pp_name fname |> error_no_info

  (* Adders *)

  let add_nodup_overloaded fid value fenv =
    if mem fid fenv then
      F.asprintf "key already exists: %a" FId.pp fid |> error_no_info;
    let fname, _ = fid in
    match find_funcs_by_name fname fenv with
    | [] -> add fid value fenv
    | values ->
        if not (List.for_all (V.eq_kind value) values) then
          F.asprintf "key already exists: %a" FId.pp fid |> error_no_info
        else add fid value fenv

  let add_nodup_non_overloaded fid value fenv =
    let fname, _ = fid in
    match find_funcs_by_name fname fenv with
    | [] -> add fid value fenv
    | _ -> F.asprintf "key already exists: %a" FId.pp fid |> error_no_info
end

module MakeCIdEnv = MakeFIdEnv

module MakeOIdEnv (V : sig
  type t

  val pp : ?level:int -> F.formatter -> t -> unit
end) =
struct
  include OIdMap

  type t = V.t OIdMap.t

  let pp ?(level = 0) fmt env = OIdMap.pp ~level V.pp fmt env

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> F.asprintf "key not found: %a" OId.pp id |> error_no_info

  let add_nodup id value env =
    if mem id env then
      F.asprintf "key already exists: %a" OId.pp id |> error_no_info
    else add id value env
end

(* Option functor *)

module MakeOption (T : sig
  type t

  val pp : ?level:int -> F.formatter -> t -> unit
  val eq : t -> t -> bool
end) =
struct
  type t = T.t option

  let pp ?(level = 0) fmt = function
    | Some t -> F.fprintf fmt "%a" (T.pp ~level) t
    | None -> F.fprintf fmt "()"

  let eq opt_a opt_b =
    match (opt_a, opt_b) with
    | Some a, Some b -> T.eq a b
    | None, None -> true
    | _ -> false
end

(* Pair functor *)

module MakePair (A : sig
  type t

  val pp : F.formatter -> t -> unit
  val eq : t -> t -> bool
end) (B : sig
  type t

  val pp : F.formatter -> t -> unit
  val eq : t -> t -> bool
end) =
struct
  type t = A.t * B.t

  let pp fmt (a, b) = F.fprintf fmt "(%a, %a)" A.pp a B.pp b
  let eq (a_1, b_1) (a_2, b_2) = A.eq a_1 a_2 && B.eq b_1 b_2
end

(* Triple functor *)

module MakeTriple (A : sig
  type t

  val pp : ?level:int -> F.formatter -> t -> unit
  val eq : t -> t -> bool
end) (B : sig
  type t

  val pp : F.formatter -> t -> unit
  val eq : t -> t -> bool
end) (C : sig
  type t

  val pp : F.formatter -> t -> unit
  val eq : t -> t -> bool
end) =
struct
  type t = A.t * B.t * C.t

  let pp ?(level = 0) fmt (a, b, c) =
    F.fprintf fmt "(%a, %a, %a)" (A.pp ~level) a B.pp b C.pp c

  let eq (a_1, b_1, c_1) (a_2, b_2, c_2) =
    A.eq a_1 a_2 && B.eq b_1 b_2 && C.eq c_1 c_2
end

(* Quad functor *)

module MakeQuad (A : sig
  type t

  val pp : ?level:int -> F.formatter -> t -> unit
  val eq : t -> t -> bool
end) (B : sig
  type t

  val pp : F.formatter -> t -> unit
  val eq : t -> t -> bool
end) (C : sig
  type t

  val pp : F.formatter -> t -> unit
  val eq : t -> t -> bool
end) (D : sig
  type t

  val pp : ?level:int -> F.formatter -> t -> unit
  val eq : t -> t -> bool
end) =
struct
  type t = A.t * B.t * C.t * D.t

  let pp ?(level = 0) fmt (a, b, c, d) =
    F.fprintf fmt "(%a, %a, %a, %a)" (A.pp ~level) a B.pp b C.pp c (D.pp ~level)
      d

  let eq (a_1, b_1, c_1, d_1) (a_2, b_2, c_2, d_2) =
    A.eq a_1 a_2 && B.eq b_1 b_2 && C.eq c_1 c_2 && D.eq d_1 d_2
end
