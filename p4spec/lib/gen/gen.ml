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

(* Take a SpecTec syntax and expand it into values*)
let rec expand_typ ?(depth=0) (tdenv : TDEnv.t) (typ : typ): value list =
  depth |> ignore;
  expand_typ' tdenv typ.it

(* Takes a list of N types and returns a list of all possible expansion combinations,
 * where each combination is a list of N values;
 *
 * If expand_typ(t_i) = [ v_(i,1); ...; v_(i,l_i) ] then
 * Result is [ [ v_(1,1); ... ; v_(N,1) ]; ... [ v_(1,l_1); ... ; v_(N;l_N) ] *)
and expand_typs tdenv typs : value list list =
  List.fold_left
    (fun (values_acc: value list list) typ -> 
       let values_t = expand_typ tdenv typ in
       List.concat_map 
         (fun (value_acc: value list) ->
            List.map 
              (fun (value_t: value) -> value_acc @ [ value_t ]) values_t)
         values_acc
    )
    [] typs

and expand_typ' tdenv typ': value list =
  match typ' with
  | BoolT -> [ BoolV true; BoolV false ]
  | NumT `NatT ->
    [ NumV (`Nat (Bigint.of_int 0)); NumV (`Nat (Bigint.of_int 4)); NumV (`Nat (Bigint.of_int 6)) ] 
  | NumT `IntT -> 
    [ NumV (`Int (Bigint.of_int 2)); NumV (`Int (Bigint.of_int (-2))) ] 
  | TextT -> [ TextV "a"; TextV "b" ]
  | TupleT typs_inner ->
    let values_combinations = expand_typs tdenv typs_inner in
    List.map (fun values -> TupleV values) values_combinations
  | IterT (typ_inner, Opt) -> 
    (* Either none or list of value candidates *)
    (* TODO: generate random list of length <= n*)
    OptV (Option.None) ::
    List.map (fun v -> OptV (Option.some v)) (expand_typ tdenv typ_inner)
  | IterT (typ_inner, List) ->
    [
      ListV ((expand_typ tdenv typ_inner))
    ]
  | FuncT -> [ ] 
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
          let values_combinations  =
            subst_typs theta typs |> expand_typs tdenv
          in
          List.map (fun values ->
              StructV (List.combine atoms values)
            ) values_combinations
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
            let values_combinations = expand_typs tdenv typs in
            List.map (fun values -> CaseV (mixop, values))
              values_combinations
          in
          List.concat_map expand_nottyp' nottyps'
      )
    | None -> []

