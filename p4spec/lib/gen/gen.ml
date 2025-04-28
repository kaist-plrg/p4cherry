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

let random_select (values : value list) : value =
  Random.int (List.length values) |> List.nth values

(* Take a SpecTec syntax and expand it into values*)
let rec expand_typ ?(depth=0) (tdenv : TDEnv.t) (typ : typ): value =
  depth |> ignore;
  expand_typ' tdenv typ.it

and expand_typs tdenv typs : value list =
  List.map (expand_typ tdenv) typs 

and expand_typ' tdenv typ': value =
  match typ' with
  | BoolT -> [ BoolV true; BoolV false ] |> random_select
  | NumT `NatT ->
    [ NumV (`Nat (Bigint.of_int 0)); NumV (`Nat (Bigint.of_int 4)); NumV (`Nat (Bigint.of_int 6)) ] |> random_select
  | NumT `IntT -> 
    [ NumV (`Int (Bigint.of_int 2)); NumV (`Int (Bigint.of_int (-2))) ] |> random_select
  | TextT -> [ TextV "a"; TextV "b" ] |> random_select
  | TupleT typs_inner ->
    TupleV (expand_typs tdenv typs_inner)
  | IterT (typ_inner, Opt) -> 
    [ OptV (Option.None); OptV (Option.some (expand_typ tdenv typ_inner)) ] |> random_select
  | IterT (typ_inner, List) ->
    let l = Random.int 3 in
      ListV (List.init l (fun _ -> expand_typ tdenv typ_inner))
  | FuncT -> FuncV ("f" $ no_region) 
  | VarT (tid, targs) -> 
    let td  = TDEnv.find_opt tid tdenv in      
    match td with
    | Some (tparams, typdef) -> (
        let theta = List.combine tparams targs |> TDEnv.of_list in
        match typdef.it with
        | PlainT typ ->
          let typ = subst_typ theta typ in
          expand_typ tdenv typ 
        | StructT typfields ->
          let atoms, typs = List.split typfields in
          let values  =
            subst_typs theta typs |> expand_typs tdenv
          in
          StructV (List.combine atoms values)
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
            let values = expand_typs tdenv typs in
            CaseV (mixop, values)
          in
          List.map expand_nottyp' nottyps' |> random_select
      )
    | None -> failwith "not found in tdenv"

let expand depth spec typ : value =
  Random.init 161;
  let tdenv = TDEnv.empty in
  let tdenv = load_spec tdenv spec in
  expand_typ ~depth tdenv typ
