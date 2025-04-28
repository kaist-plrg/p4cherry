open Il.Ast
open Runtime_dynamic_il.Envs
open Runtime_dynamic_il.Typ
open Util.Source

(* Load definitions into environment *)
let load_def (tdenv: TDEnv.t) (def : def) : TDEnv.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
    let typdef = (tparams, deftyp) in
    TDEnv.add id typdef tdenv
  | _ -> tdenv

let load_spec (tdenv: TDEnv.t) (spec : spec) : TDEnv.t =
  List.fold_left load_def tdenv spec

let random_select (values : value list) : value option =
  Random.int (List.length values) |> List.nth values |> Option.some

let ( let* ) x f = Option.bind x f

let bind_list (opt_list : value option list) : value list option =
    if opt_list |> List.exists (Option.is_none) then None
    else opt_list |> List.map (Option.get) |> Option.some

(* Take a SpecTec syntax and expand it into values*)
let rec expand_typ depth (tdenv : TDEnv.t) (typ : typ): value option =
  if depth <= 0 then None
  else
  expand_typ' depth tdenv typ.it

and expand_typs depth tdenv typs : value list option=
  if depth <= 0 then None
  else List.map (expand_typ depth tdenv) typs |> bind_list

and expand_typ' depth tdenv typ': value option =
  let depth = depth-1 in
  match typ' with
  | BoolT -> [ BoolV true; BoolV false ] |> random_select
  | NumT `NatT ->
    [ NumV (`Nat (Bigint.of_int 0)); NumV (`Nat (Bigint.of_int 4)); NumV (`Nat (Bigint.of_int 6)) ] |> random_select
  | NumT `IntT -> 
    [ NumV (`Int (Bigint.of_int 2)); NumV (`Int (Bigint.of_int (-2))) ] |> random_select
  | TextT -> [ TextV "a"; TextV "b" ] |> random_select
  | TupleT typs_inner ->
    let* values_inner = expand_typs depth tdenv typs_inner in
    TupleV values_inner |> Option.some
  | IterT (typ_inner, Opt) -> 
    let i = Random.int 2 in
    if depth = 0 || i = 0 then OptV (None) |> Option.some
    else
      (let* value_inner = expand_typ depth tdenv typ_inner in
      OptV (Some value_inner) |> Option.some)
  | IterT (typ_inner, List) ->
    if depth = 0 then ListV ([]) |> Option.some
    else
      let max_length = 3 in
      let l = Random.int max_length in
      let* values_inner = List.init l (fun _ -> expand_typ depth tdenv typ_inner) |> bind_list in
      ListV (values_inner) |> Option.some
  (* Does not expand FuncT *)
  | FuncT -> None
  | VarT (tid, targs) -> 
    let td  = TDEnv.find_opt tid tdenv in      
    match td with
    | Some (tparams, typdef) -> (
        let theta = List.combine tparams targs |> TDEnv.of_list in
        match typdef.it with
        | PlainT typ ->
          let typ = subst_typ theta typ in
          expand_typ depth tdenv typ 
        | StructT typfields ->
          let atoms, typs = List.split typfields in
          let* values =
            typs |> subst_typs theta |> expand_typs depth tdenv
          in
          StructV (List.combine atoms values) |> Option.some
        | VariantT typcases -> 
          let nottyps' = List.map it typcases in
          let nottyps' = 
            List.map (fun (mixop, typs) -> 
              let typs = subst_typs theta typs in
              (mixop, typs))
              nottyps'
          in
          let expand_nottyp' nottyp'= 
            let mixop, typs = nottyp' in
            let* values = expand_typs depth tdenv typs in
            CaseV (mixop, values) |> Option.some
          in
          (* randomly selects from only successful CaseVs *)
          List.map expand_nottyp' nottyps' |> List.filter (Option.is_some) |> List.map (Option.get) |> random_select
      )
    (* VarT not found in environment *)
    | None -> None

let expand depth spec typ : value option =
  Random.init 161;
  let tdenv = TDEnv.empty in
  let tdenv = load_spec tdenv spec in
  expand_typ depth tdenv typ
