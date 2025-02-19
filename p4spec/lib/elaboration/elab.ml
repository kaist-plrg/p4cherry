open Xl
open El.Ast
open Dom
open Attempt
open Util.Source

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let distinct (eq : 'a -> 'a -> bool) (xs : 'a list) : bool =
  let rec distinct' xs =
    match xs with
    | [] -> true
    | x :: xs -> if List.exists (eq x) xs then false else distinct' xs
  in
  distinct' xs

(* Todo *)

let todo (func : string) (msg : string) =
  let msg = Format.asprintf "(TODO : %s) %s\n" func msg in
  fail no_region msg

(* Parentheses handling *)

let rec unparen_plaintyp (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | ParenT plaintyp -> unparen_plaintyp plaintyp
  | _ -> plaintyp

let rec unparen_exp (exp : exp) : exp =
  match exp.it with ParenE exp -> unparen_exp exp | _ -> exp

(* Identifiers *)

let strip_var_suffix id =
  let rec is_sub id idx =
    idx = String.length id || (id.[idx] = '_' && is_sub id (idx + 1))
  in
  match (String.index_opt id.it '_', String.index_opt id.it '\'') with
  | None, None -> id
  | Some idx, None when is_sub id.it idx -> id
  | None, Some idx | Some idx, None -> String.sub id.it 0 idx $ id.at
  | Some idx_a, Some idx_b -> String.sub id.it 0 (min idx_a idx_b) $ id.at

let valid_tid (id : id) = id.it = (strip_var_suffix id).it

(* Iteration *)

let elab_iter (iter : iter) : Il.Ast.iter =
  match iter with Opt -> Il.Ast.Opt | List -> Il.Ast.List

(* Types *)

type kind =
  [ `Plain
  | `Notation of nottyp
  | `Struct of typfield list
  | `Variant of typcase list ]

let kind_of_typ (ctx : Ctx.t) (typ : Il.Ast.typ) : kind =
  match typ.it with
  | VarT (tid, _) -> (
      let td = Ctx.find_typdef ctx tid in
      match td with
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | NotationT nottyp -> `Notation nottyp
          | StructT typfields -> `Struct typfields
          | VariantT typcases -> `Variant typcases)
      | _ -> `Plain)
  | _ -> `Plain

(* Expansion of aliases *)

let rec expand_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | VarT (tid, _) -> (
      let td = Ctx.find_typdef ctx tid in
      match td with
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | NotationT { it = PlainT plaintyp; _ } ->
              expand_plaintyp ctx plaintyp
          | _ -> plaintyp)
      | _ -> plaintyp)
  | _ -> plaintyp

(* Type equivalence and subtyping *)

let rec equiv_plaintyp (ctx : Ctx.t) (plaintyp_a : plaintyp)
    (plaintyp_b : plaintyp) : bool =
  let plaintyp_a = expand_plaintyp ctx plaintyp_a in
  let plaintyp_b = expand_plaintyp ctx plaintyp_b in
  match (plaintyp_a.it, plaintyp_b.it) with
  | BoolT, BoolT -> true
  | NumT numtyp_a, NumT numtyp_b -> Num.equiv numtyp_a numtyp_b
  | TextT, TextT -> true
  | VarT (tid_a, targs_a), VarT (tid_b, targs_b) ->
      tid_a.it = tid_b.it
      && List.length targs_a = List.length targs_b
      && List.for_all2 (equiv_plaintyp ctx) targs_a targs_b
  | ParenT plaintyp_a, _ -> equiv_plaintyp ctx plaintyp_a plaintyp_b
  | _, ParenT plaintyp_b -> equiv_plaintyp ctx plaintyp_a plaintyp_b
  | TupleT plaintyps_a, TupleT plaintyps_b ->
      List.length plaintyps_a = List.length plaintyps_b
      && List.for_all2 (equiv_plaintyp ctx) plaintyps_a plaintyps_b
  | IterT (plaintyp_a, iter_a), IterT (plaintyp_b, iter_b) ->
      equiv_plaintyp ctx plaintyp_a plaintyp_b && iter_a = iter_b
  | _ -> false

let rec sub_plaintyp (ctx : Ctx.t) (plaintyp_a : plaintyp)
    (plaintyp_b : plaintyp) : bool =
  equiv_plaintyp ctx plaintyp_a plaintyp_b
  || sub_plaintyp' ctx plaintyp_a plaintyp_b

and sub_plaintyp' (ctx : Ctx.t) (plaintyp_a : plaintyp) (plaintyp_b : plaintyp)
    : bool =
  let plaintyp_a = expand_plaintyp ctx plaintyp_a in
  let plaintyp_b = expand_plaintyp ctx plaintyp_b in
  match (plaintyp_a.it, plaintyp_b.it) with
  | NumT numtyp_a, NumT numtyp_b -> Num.sub numtyp_a numtyp_b
  | ParenT plaintyp_a, _ -> sub_plaintyp ctx plaintyp_a plaintyp_b
  | _, ParenT plaintyp_b -> sub_plaintyp ctx plaintyp_a plaintyp_b
  | TupleT plaintyps_a, TupleT plaintyps_b ->
      List.length plaintyps_a = List.length plaintyps_b
      && List.for_all2 (sub_plaintyp ctx) plaintyps_a plaintyps_b
  | IterT (plaintyp_a, iter_a), IterT (plaintyp_b, iter_b) when iter_a = iter_b
    ->
      sub_plaintyp ctx plaintyp_a plaintyp_b
  | IterT (plaintyp_a, Opt), IterT (plaintyp_b, List) ->
      sub_plaintyp ctx plaintyp_a plaintyp_b
  | _, IterT (plaintyp_b, List) -> sub_plaintyp ctx plaintyp_a plaintyp_b
  | _ -> false

(* Plain types *)

let rec elab_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : Il.Ast.typ =
  let typ_il = elab_plaintyp' ctx plaintyp.it in
  typ_il $ plaintyp.at

and elab_plaintyp' (ctx : Ctx.t) (plaintyp : plaintyp') : Il.Ast.typ' =
  match plaintyp with
  | BoolT -> Il.Ast.BoolT
  | NumT numtyp -> Il.Ast.NumT numtyp
  | TextT -> Il.Ast.TextT
  | VarT (tid, targs) ->
      let td = Ctx.find_typdef ctx tid in
      let typs_il = List.map (elab_plaintyp ctx) targs in
      let tparams = TypeDef.get_tparams td in
      check
        (List.length tparams = List.length targs)
        tid.at "type arguments do not match";
      Il.Ast.VarT (tid, typs_il)
  | ParenT plaintyp -> elab_plaintyp' ctx plaintyp.it
  | TupleT plaintyps ->
      let typs_il = List.map (elab_plaintyp ctx) plaintyps in
      Il.Ast.TupleT typs_il
  | IterT (plaintyp, iter) ->
      let typ_il = elab_plaintyp ctx plaintyp in
      let iter_il = elab_iter iter in
      Il.Ast.IterT (typ_il, iter_il)

(* Notation types *)

and elab_nottyp (ctx : Ctx.t) (nottyp : nottyp) : Il.Ast.nottyp =
  let nottyp_il = elab_nottyp' ctx nottyp.it in
  nottyp_il $ nottyp.at

and elab_nottyp' (ctx : Ctx.t) (nottyp : nottyp') : Il.Ast.nottyp' =
  match nottyp with
  | PlainT plaintyp ->
      let mixop = [ []; [] ] in
      let typ_il = elab_plaintyp ctx plaintyp in
      (mixop, [ typ_il ])
  | AtomT atom ->
      let mixop = [ [ atom ] ] in
      let typs_il = [] in
      (mixop, typs_il)
  | SeqT [] ->
      let mixop = [ [] ] in
      let typs_il = [] in
      (mixop, typs_il)
  | SeqT (nottyp :: nottyps) ->
      let mixop_h, typs_il_h = elab_nottyp' ctx nottyp.it in
      let mixop_t, typs_il_t = elab_nottyp' ctx (SeqT nottyps) in
      let mixop = Mixop.merge mixop_h mixop_t in
      let typs_il = typs_il_h @ typs_il_t in
      (mixop, typs_il)
  | InfixT (nottyp_l, atom, nottyp_r) ->
      let mixop_l, typs_il_l = elab_nottyp' ctx nottyp_l.it in
      let mixop_r, typs_il_r = elab_nottyp' ctx nottyp_r.it in
      let mixop_l = Mixop.merge mixop_l [ [ atom ] ] in
      let mixop = Mixop.merge mixop_l mixop_r in
      let typs_il = typs_il_l @ typs_il_r in
      (mixop, typs_il)
  | BrackT (atom_l, nottyp, atom_r) ->
      let mixop, typs_il = elab_nottyp' ctx nottyp.it in
      let mixop_l = Mixop.merge [ [ atom_l ] ] mixop in
      let mixop = Mixop.merge mixop_l [ [ atom_r ] ] in
      (mixop, typs_il)

(* Definition types *)

and elab_deftyp (ctx : Ctx.t) (deftyp : deftyp) : Il.Ast.deftyp =
  match deftyp.it with
  | NotationT nottyp -> elab_typ_def_notation ctx nottyp
  | StructT typfields -> elab_typ_def_struct ctx deftyp.at typfields
  | VariantT typcases -> elab_typ_def_variant ctx deftyp.at typcases

(* Notation type definitions *)

and elab_typ_def_notation (ctx : Ctx.t) (nottyp : nottyp) : Il.Ast.deftyp =
  match nottyp.it with
  | PlainT plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.AliasT typ_il $ nottyp.at
  | _ ->
      let nottyp_il = elab_nottyp ctx nottyp in
      Il.Ast.NotationT nottyp_il $ nottyp.at

(* Struct type definitions *)

and elab_typ_def_struct (ctx : Ctx.t) (at : region) (typfields : typfield list)
    : Il.Ast.deftyp =
  let typfields_il = List.map (elab_typfield ctx) typfields in
  let deftyp_il = Il.Ast.StructT typfields_il in
  deftyp_il $ at

and elab_typfield (ctx : Ctx.t) (typfield : typfield) : Il.Ast.typfield =
  let atom, plaintyp, _hints = typfield in
  let typ_il = elab_plaintyp ctx plaintyp in
  (atom, typ_il)

(* Variant type definitions *)

and elab_typcase (ctx : Ctx.t) (typcase : typcase) : Il.Ast.typcase =
  let nottyp, _hints = typcase in
  elab_nottyp ctx nottyp

and elab_typ_def_variant (ctx : Ctx.t) (at : region) (typcases : typcase list) :
    Il.Ast.deftyp =
  let typcases_il = List.map (elab_typcase ctx) typcases in
  let mixops = typcases_il |> List.map it |> List.map fst in
  check (distinct Mixop.eq mixops) no_region "cases are ambiguous";
  let deftyp_il = Il.Ast.VariantT typcases_il in
  deftyp_il $ at

(* Expressions *)

(* Expression type inference *)

and infer_as_list (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp attempt =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, List) -> Ok plaintyp
  | _ -> fail plaintyp.at "cannot infer type as list"

and infer_as_struct (ctx : Ctx.t) (plaintyp : plaintyp) : typfield list attempt
    =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | VarT (tid, _) -> (
      let td_opt = Ctx.find_typdef_opt ctx tid in
      match td_opt with
      | Some (Defined (_, { it = StructT typfields; _ })) -> Ok typfields
      | _ -> fail plaintyp.at "cannot infer type as struct")
  | _ -> fail plaintyp.at "cannot infer type as struct"

and fail_infer (at : region) (construct : string) =
  fail at ("cannot infer type of " ^ construct)

and infer_exp (ctx : Ctx.t) (exp : exp) : (Il.Ast.exp * plaintyp) attempt =
  let* exp_il, plaintyp = infer_exp' ctx exp.at exp.it in
  let typ_il = elab_plaintyp ctx (plaintyp $ exp.at) in
  Ok (exp_il $$ (exp.at, typ_il.it), plaintyp $ exp.at)

and infer_exp' (ctx : Ctx.t) (at : region) (exp : exp') :
    (Il.Ast.exp' * plaintyp') attempt =
  match exp with
  | BoolE b -> infer_bool_exp b
  | NumE (_, num) -> infer_num_exp num
  | TextE text -> infer_text_exp text
  | VarE (id, targs) -> infer_var_exp ctx id targs
  | UnE _ -> todo "infer_exp" "UnE"
  | BinE _ -> todo "infer_exp" "BinE"
  | CmpE (exp_l, cmpop, exp_r) -> infer_cmpop_exp ctx at cmpop exp_l exp_r
  | ArithE exp -> infer_arith_exp ctx exp
  | EpsE -> fail_infer at "empty sequence"
  | ListE exps -> infer_list_exp ctx at exps
  | ConsE (exp_h, exp_t) -> infer_cons_exp ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> infer_cat_exp ctx exp_l exp_r
  | IdxE (exp_b, exp_i) -> infer_idx_exp ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> infer_slice_exp ctx exp_b exp_l exp_h
  | LenE exp -> infer_len_exp ctx exp
  | MemE (exp_e, exp_s) -> infer_mem_exp ctx exp_e exp_s
  | StrE _ -> fail_infer at "struct expression"
  | DotE (exp, atom) -> infer_dot_exp ctx exp atom
  | UpdE (exp_b, path, exp_f) -> infer_upd_exp ctx exp_b path exp_f
  | ParenE exp -> infer_paren_exp ctx exp
  | TupleE exps -> infer_tuple_exp ctx exps
  | CallE (id, targs, args) -> infer_call_exp ctx id targs args
  | IterE (exp, iter) -> infer_iter_exp ctx exp iter
  | TypE (exp, plaintyp) -> infer_typ_exp ctx exp plaintyp
  | AtomE _ -> fail_infer at "atom"
  | SeqE _ -> fail_infer at "sequence expression"
  | InfixE _ -> fail_infer at "infix expression"
  | BrackE _ -> fail_infer at "bracket expression"
  | HoleE _ -> error at "misplaced hole"
  | FuseE _ -> error at "misplaced token concatenation"
  | UnparenE _ -> error at "misplaced unparenthesize"
  | LatexE _ -> error at "misplaced LaTeX literal"

and infer_exps (ctx : Ctx.t) (exps : exp list) :
    (Il.Ast.exp list * plaintyp list) attempt =
  match exps with
  | [] -> Ok ([], [])
  | exp :: exps ->
      let* exp_il, plaintyp = infer_exp ctx exp in
      let* exps_il, plaintyps = infer_exps ctx exps in
      Ok (exp_il :: exps_il, plaintyp :: plaintyps)

(* Boolean expressions *)

and infer_bool_exp (b : bool) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.BoolE b in
  let plaintyp = BoolT in
  Ok (exp_il, plaintyp)

(* Number expressions *)

and infer_num_exp (num : Num.t) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.NumE num in
  let plaintyp = NumT (Num.to_typ num) in
  Ok (exp_il, plaintyp)

(* Text expressions *)

and infer_text_exp (text : string) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.TextE text in
  let plaintyp = TextT in
  Ok (exp_il, plaintyp)

(* Variable expressions *)

and infer_var_exp (ctx : Ctx.t) (id : id) (targs : targ list) :
    (Il.Ast.exp' * plaintyp') attempt =
  let tid = strip_var_suffix id in
  let meta_opt = Ctx.find_metavar_opt ctx tid in
  match meta_opt with
  | Some _ when targs <> [] ->
      fail_infer id.at "meta-variable with type arguments is disallowed"
  | Some plaintyp ->
      let exp_il = Il.Ast.VarE id in
      Ok (exp_il, plaintyp.it)
  | None ->
      let exp_il = Il.Ast.VarE id in
      let plaintyp = VarT (tid, targs) in
      Ok (exp_il, plaintyp)

(* Comparison expressions *)

and infer_cmpop_exp_poly (ctx : Ctx.t) (at : region) (cmpop : Bool.cmpop)
    (exp_l : exp) (exp_r : exp) : (Il.Ast.exp' * plaintyp') attempt =
  choice at
    [
      (fun () ->
        let* exp_il_r, plaintyp_r = infer_exp ctx exp_r in
        let* exp_il_l = elab_exp ctx plaintyp_r exp_l in
        let exp_il =
          Il.Ast.CmpE ((cmpop :> Il.Ast.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (exp_il, BoolT));
      (fun () ->
        let* exp_il_l, plaintyp_l = infer_exp ctx exp_l in
        let* exp_il_r = elab_exp ctx plaintyp_l exp_r in
        let exp_il =
          Il.Ast.CmpE ((cmpop :> Il.Ast.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (exp_il, BoolT));
    ]

and infer_cmpop_exp (ctx : Ctx.t) (at : region) (cmpop : cmpop) (exp_l : exp)
    (exp_r : exp) : (Il.Ast.exp' * plaintyp') attempt =
  match cmpop with
  | #Bool.cmpop as cmpop -> infer_cmpop_exp_poly ctx at cmpop exp_l exp_r
  | #Num.cmpop -> todo "infer_cmpop_exp" "Num.cmpop"

(* List expressions *)

and infer_list_exp (ctx : Ctx.t) (at : region) (exps : exp list) :
    (Il.Ast.exp' * plaintyp') attempt =
  match exps with
  | [] -> fail_infer at "empty list"
  | exp :: exps ->
      let* exp_il, plaintyp = infer_exp ctx exp in
      let* exps_il, plaintyps = infer_exps ctx exps in
      if List.for_all (equiv_plaintyp ctx plaintyp) plaintyps then
        let exp_il = Il.Ast.ListE (exp_il :: exps_il) in
        let plaintyp = IterT (plaintyp, List) in
        Ok (exp_il, plaintyp)
      else fail_infer at "list with heterogeneous elements"

(* Cons expressions *)

and infer_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_h, plaintyp_h = infer_exp ctx exp_h in
  let plaintyp = IterT (plaintyp_h, List) in
  let* exp_il_t = elab_exp ctx (plaintyp $ plaintyp_h.at) exp_t in
  let exp_il = Il.Ast.ConsE (exp_il_h, exp_il_t) in
  Ok (exp_il, plaintyp)

(* Concatenation expressions *)

and infer_cat_exp (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_l, plaintyp_l = infer_exp ctx exp_l in
  let* plaintyp = infer_as_list ctx plaintyp_l in
  let plaintyp = IterT (plaintyp, List) $ plaintyp.at in
  let* exp_il_r = elab_exp ctx plaintyp exp_r in
  let exp_il = Il.Ast.CatE (exp_il_l, exp_il_r) in
  Ok (exp_il, plaintyp.it)

(* Index expressions *)

and infer_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* plaintyp = infer_as_list ctx plaintyp_b in
  let* exp_il_i = elab_exp ctx (NumT `NatT $ exp_i.at) exp_i in
  let exp_il = Il.Ast.IdxE (exp_il_b, exp_il_i) in
  Ok (exp_il, plaintyp.it)

(* Slice expressions *)

and infer_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_l : exp) (exp_h : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* plaintyp = infer_as_list ctx plaintyp_b in
  let* exp_il_l = elab_exp ctx (NumT `NatT $ exp_l.at) exp_l in
  let* exp_il_h = elab_exp ctx (NumT `NatT $ exp_h.at) exp_h in
  let exp_il = Il.Ast.SliceE (exp_il_b, exp_il_l, exp_il_h) in
  Ok (exp_il, plaintyp.it)

(* Member expressions *)

and infer_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_e, plaintyp_e = infer_exp ctx exp_e in
  let* exp_il_s =
    elab_exp ctx (IterT (plaintyp_e, List) $ plaintyp_e.at) exp_s
  in
  let exp_il = Il.Ast.MemE (exp_il_e, exp_il_s) in
  let plaintyp = BoolT in
  Ok (exp_il, plaintyp)

(* Dot expressions *)

and infer_dot_exp (ctx : Ctx.t) (exp : exp) (atom : atom) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let* typfields = infer_as_struct ctx plaintyp in
  let* plaintyp =
    List.find_opt (fun (atom_t, _, _) -> atom.it = atom_t.it) typfields
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, plaintyp, _) -> Ok plaintyp
    | None -> fail exp.at "cannot infer type of field"
  in
  let exp_il = Il.Ast.DotE (exp_il, atom) in
  Ok (exp_il, plaintyp.it)

(* Update expressions *)

and infer_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* path_il, plaintyp_f = elab_path ctx plaintyp_b path in
  let* exp_il_f = elab_exp ctx plaintyp_f exp_f in
  let exp_il = Il.Ast.UpdE (exp_il_b, path_il, exp_il_f) in
  Ok (exp_il, plaintyp_b.it)

(* Length expressions *)

and infer_len_exp (ctx : Ctx.t) (exp : exp) : (Il.Ast.exp' * plaintyp') attempt
    =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let* _plaintyp = infer_as_list ctx plaintyp in
  let exp_il = Il.Ast.LenE exp_il in
  let plaintyp = NumT `NatT in
  Ok (exp_il, plaintyp)

(* Parenthesized expressions *)

and infer_paren_exp (ctx : Ctx.t) (exp : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  infer_exp' ctx exp.at exp.it

(* Tuple expressions *)

and infer_tuple_exp (ctx : Ctx.t) (exps : exp list) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exps_il, plaintyps = infer_exps ctx exps in
  let exp_il = Il.Ast.TupleE exps_il in
  let plaintyp = TupleT plaintyps in
  Ok (exp_il, plaintyp)

(* Call expressions *)

and infer_call_exp (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
    : (Il.Ast.exp' * plaintyp') attempt =
  let tparams, params, plaintyp = Ctx.find_dec ctx id in
  check
    (List.length targs = List.length tparams)
    id.at "type arguments do not match";
  let targs_il = List.map (elab_plaintyp ctx) targs in
  check (List.length args = List.length params) id.at "arguments do not match";
  let args_il = List.map2 (elab_arg ctx) params args in
  let exp_il = Il.Ast.CallE (id, targs_il, args_il) in
  Ok (exp_il, plaintyp.it)

(* Iterated expressions *)

and infer_iter_exp (ctx : Ctx.t) (exp : exp) (iter : iter) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let iter_il = elab_iter iter in
  let exp_il = Il.Ast.IterE (exp_il, (iter_il, [])) in
  let plaintyp = IterT (plaintyp, iter) in
  Ok (exp_il, plaintyp)

(* Typed expressions *)

and infer_typ_exp (ctx : Ctx.t) (exp : exp) (plaintyp : plaintyp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il = elab_exp ctx plaintyp exp in
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ok (exp_il.it, plaintyp.it)

(* Arithmetic expressions *)

and infer_arith_exp (ctx : Ctx.t) (exp : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  infer_exp' ctx exp.at exp.it

(* Expression type elaboration *)

and fail_cast (at : region) (plaintyp_a : plaintyp) (plaintyp_b : plaintyp) =
  Fail
    ( at,
      "cannot cast "
      ^ El.Print.string_of_plaintyp plaintyp_a
      ^ " to "
      ^ El.Print.string_of_plaintyp plaintyp_b )

and cast_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp)
    (plaintyp_infer : plaintyp) (exp_il : Il.Ast.exp) : Il.Ast.exp attempt =
  if equiv_plaintyp ctx plaintyp_expect plaintyp_infer then Ok exp_il
  else if sub_plaintyp ctx plaintyp_infer plaintyp_expect then
    let typ_il_expect = elab_plaintyp ctx plaintyp_expect in
    let exp_il =
      Il.Ast.CastE (exp_il, typ_il_expect) $$ (exp_il.at, typ_il_expect.it)
    in
    Ok exp_il
  else fail_cast exp_il.at plaintyp_infer plaintyp_expect

and elab_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    Il.Ast.exp attempt =
  (* Expression elaboration is a two-step process:
     - if a type can be inferred without any contextual information,
       match the inferred type with the expected type
     - this may fail for some expressions that require contextual information,
       e.g., notation expressions or expression sequences
     - for such cases, try to elaborate the expression using the expected type *)
  let infer_attempt = infer_exp ctx exp in
  match infer_attempt with
  | Ok (exp_il, plaintyp_infer) ->
      cast_exp ctx plaintyp_expect plaintyp_infer exp_il
  | Fail _ -> (
      let typ_il = elab_plaintyp ctx plaintyp_expect in
      let kind = kind_of_typ ctx typ_il in
      match kind with
      | `Plain -> elab_exp_plain ctx plaintyp_expect exp
      | `Notation nottyp ->
          let* nottyp_il = elab_exp_not ctx nottyp exp in
          Ok (Il.Ast.CaseE nottyp_il $$ (exp.at, typ_il.it))
      | `Struct typfields ->
          let* expfields_il = elab_exp_struct ctx typfields exp in
          Ok (Il.Ast.StrE expfields_il $$ (exp.at, typ_il.it))
      | `Variant typcases ->
          let* nottyp_il = elab_exp_variant ctx typcases exp in
          Ok (Il.Ast.CaseE nottyp_il $$ (exp.at, typ_il.it)))

and elab_exps (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exps : exp list) :
    Il.Ast.exp list attempt =
  match exps with
  | [] -> Ok []
  | exp :: exps ->
      let* exp_il = elab_exp ctx plaintyp_expect exp in
      let* exps_il = elab_exps ctx plaintyp_expect exps in
      Ok (exp_il :: exps_il)

(* Plain expressions *)

and elab_as_iter (ctx : Ctx.t) (plaintyp : plaintyp) : (plaintyp * iter) attempt
    =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, iter) -> Ok (plaintyp, iter)
  | _ -> fail plaintyp.at "cannot elaborate type as an iteration"

and elab_as_list (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp attempt =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, List) -> Ok plaintyp
  | _ -> fail plaintyp.at "cannot elaborate type as a list"

and elab_as_struct (ctx : Ctx.t) (plaintyp : plaintyp) : typfield list attempt =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | VarT (tid, _) -> (
      let td_opt = Ctx.find_typdef_opt ctx tid in
      match td_opt with
      | Some (Defined (_, { it = StructT typfields; _ })) -> Ok typfields
      | _ -> fail plaintyp.at "cannot elaborate type as struct")
  | _ -> fail plaintyp.at "cannot elaborate type as struct"

and fail_elab_plain (at : region) (msg : string) =
  fail at ("cannot elaborate expression because" ^ msg)

and elab_exp_plain (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    Il.Ast.exp attempt =
  let* exp_il = elab_exp_plain' ctx exp.at plaintyp_expect exp.it in
  let typ_il = elab_plaintyp ctx plaintyp_expect in
  Ok (exp_il $$ (exp.at, typ_il.it))

and elab_exp_plain' (ctx : Ctx.t) (at : region) (plaintyp_expect : plaintyp)
    (exp : exp') : Il.Ast.exp' attempt =
  match exp with
  | BoolE _ | NumE _ | TextE _ | VarE _ ->
      fail_elab_plain at "should have been inferred"
  | EpsE -> elab_eps_exp ctx plaintyp_expect
  | ListE exps -> elab_list_exp ctx plaintyp_expect exps
  | ConsE (exp_h, exp_t) -> elab_cons_exp ctx plaintyp_expect exp_h exp_t
  | CatE (exp_l, exp_r) -> elab_cat_exp ctx plaintyp_expect exp_l exp_r
  | ParenE exp -> elab_paren_exp ctx plaintyp_expect exp
  | IterE (exp, iter) -> elab_iter_exp ctx plaintyp_expect exp iter
  | _ -> todo "elab_exp_plain" (El.Print.string_of_exp (exp $ at))

(* Episilon expressions *)

and elab_eps_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) :
    Il.Ast.exp' attempt =
  let* _plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  match iter_expect with
  | Opt -> Ok (Il.Ast.OptE None)
  | List -> Ok (Il.Ast.ListE [])

(* List expressions *)

and elab_list_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exps : exp list) :
    Il.Ast.exp' attempt =
  let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  match iter_expect with
  | Opt -> fail_elab_plain no_region "list expression with optional iteration"
  | List ->
      let* exps_il = elab_exps ctx plaintyp_expect exps in
      Ok (Il.Ast.ListE exps_il)

(* Cons expressions *)

and elab_cons_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp_h : exp)
    (exp_t : exp) : Il.Ast.exp' attempt =
  let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  let* exp_il_h = elab_exp ctx plaintyp_expect exp_h in
  let* exp_il_t =
    elab_exp ctx
      (IterT (plaintyp_expect, iter_expect) $ plaintyp_expect.at)
      exp_t
  in
  Ok (Il.Ast.ConsE (exp_il_h, exp_il_t))

(* Concatenation expressions *)

and elab_cat_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp_l : exp)
    (exp_r : exp) : Il.Ast.exp' attempt =
  let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  let plaintyp_expect =
    IterT (plaintyp_expect, iter_expect) $ plaintyp_expect.at
  in
  let* exp_il_l = elab_exp ctx plaintyp_expect exp_l in
  let* exp_il_r = elab_exp ctx plaintyp_expect exp_r in
  Ok (Il.Ast.CatE (exp_il_l, exp_il_r))

(* Parenthesized expressions *)

and elab_paren_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    Il.Ast.exp' attempt =
  let* exp_il = elab_exp ctx plaintyp_expect exp in
  Ok exp_il.it

(* Iterated expressions *)

and elab_iter_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp)
    (iter : iter) : Il.Ast.exp' attempt =
  let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  if iter <> iter_expect then fail_elab_plain exp.at "iteration mismatch"
  else
    let* exp_il = elab_exp ctx plaintyp_expect exp in
    let iter_il_expect = elab_iter iter_expect in
    Ok (Il.Ast.IterE (exp_il, (iter_il_expect, [])))

(* Notation expressions *)

and fail_elab_not (at : region) (msg : string) : Il.Ast.notexp attempt =
  Fail (at, "cannot elaborate notation expression because" ^ msg)

and elab_exp_not (ctx : Ctx.t) (nottyp : nottyp) (exp : exp) :
    Il.Ast.notexp attempt =
  let exp = unparen_exp exp in
  match (nottyp.it, exp.it) with
  | PlainT plaintyp, _ ->
      let mixop = [ []; [] ] in
      let* exp_il = elab_exp ctx plaintyp exp in
      Ok (mixop, [ exp_il ])
  | AtomT atom_t, AtomE atom_e when atom_t.it <> atom_e.it ->
      fail_elab_not exp.at "atom does not match"
  | AtomT atom_t, AtomE _ ->
      let mixop = [ [ atom_t ] ] in
      Ok (mixop, [])
  | SeqT [], SeqE [] ->
      let mixop = [ [] ] in
      let exps_il = [] in
      Ok (mixop, exps_il)
  | SeqT (nottyp :: nottyps), SeqE (exp :: exps) ->
      let* mixop_h, exps_il_h = elab_exp_not ctx nottyp exp in
      let* mixop_t, exps_il_t =
        elab_exp_not ctx (SeqT nottyps $ nottyp.at) (SeqE exps $ exp.at)
      in
      let mixop = Mixop.merge mixop_h mixop_t in
      let exps_il = exps_il_h @ exps_il_t in
      Ok (mixop, exps_il)
  | SeqT (_ :: _), SeqE [] -> fail_elab_not exp.at "omitted sequence tail"
  | SeqT [], SeqE (_ :: _) -> fail_elab_not exp.at "expression is not empty"
  | InfixT (_, atom_t, _), InfixE (_, atom_e, _) when atom_t.it <> atom_e.it ->
      fail_elab_not exp.at "atoms do not match"
  | InfixT (nottyp_l, atom_t, nottyp_r), InfixE (exp_l, _, exp_r) ->
      let* mixop_l, exps_il_l = elab_exp_not ctx nottyp_l exp_l in
      let* mixop_r, exps_il_r = elab_exp_not ctx nottyp_r exp_r in
      let mixop_l = Mixop.merge mixop_l [ [ atom_t ] ] in
      let mixop = Mixop.merge mixop_l mixop_r in
      let exps_il = exps_il_l @ exps_il_r in
      Ok (mixop, exps_il)
  | BrackT (atom_t_l, _, atom_t_r), BrackE (atom_e_l, _, atom_e_r)
    when atom_t_l.it <> atom_e_l.it || atom_t_r.it <> atom_e_r.it ->
      fail_elab_not exp.at "atoms do not match"
  | BrackT (atom_t_l, nottyp, atom_t_r), BrackE (_, exp, _) ->
      let* mixop, exps_il = elab_exp_not ctx nottyp exp in
      let mixop_l = Mixop.merge [ [ atom_t_l ] ] mixop in
      let mixop = Mixop.merge mixop_l [ [ atom_t_r ] ] in
      Ok (mixop, exps_il)
  | _ -> fail_elab_not exp.at "expression does not match notation"

(* Struct expressions *)

and fail_elab_struct (at : region) (msg : string) :
    (Il.Ast.atom * Il.Ast.exp) list attempt =
  Fail (at, "cannot elaborate struct expression because" ^ msg)

and elab_expfields (ctx : Ctx.t) (at : region)
    (typfields : (atom * plaintyp) list) (expfields : (atom * exp) list) :
    (Il.Ast.atom * Il.Ast.exp) list attempt =
  match (typfields, expfields) with
  | [], [] -> Ok []
  | [], (atom_e, _) :: _ ->
      fail_elab_struct atom_e.at "expression has extra fields"
  | _ :: _, [] -> fail_elab_struct at "expression omitted struct fields"
  | (atom_t, _) :: _, (atom_e, _) :: _ when atom_t.it <> atom_e.it ->
      fail_elab_struct atom_e.at "atom does not match"
  | (atom_t, plaintyp) :: typfields, (_, exp) :: expfields ->
      let* exp_il = elab_exp ctx plaintyp exp in
      let* expfields_il = elab_expfields ctx at typfields expfields in
      Ok ((atom_t, exp_il) :: expfields_il)

and elab_exp_struct (ctx : Ctx.t) (typfields : typfield list) (exp : exp) :
    (Il.Ast.atom * Il.Ast.exp) list attempt =
  let typfields =
    List.map (fun (atom, plaintyp, _) -> (atom, plaintyp)) typfields
  in
  match exp.it with
  | StrE expfields ->
      let* expfields_il = elab_expfields ctx exp.at typfields expfields in
      Ok expfields_il
  | _ -> fail_elab_struct exp.at "expression is not a struct"

(* Variant expressions *)

and fail_elab_variant (at : region) (msg : string) : Il.Ast.notexp attempt =
  fail at ("cannot elaborate variant case because" ^ msg)

and elab_exp_variant (ctx : Ctx.t) (typcases : typcase list) (exp : exp) :
    Il.Ast.notexp attempt =
  let notexps_il =
    List.filter_map
      (fun (nottyp, _) ->
        let notexp_il_attempt = elab_exp_not ctx nottyp exp in
        match notexp_il_attempt with
        | Ok notexp_il -> Some notexp_il
        | Fail _ -> None)
      typcases
  in
  match notexps_il with
  | [ notexp_il ] -> Ok notexp_il
  | [] -> fail_elab_variant exp.at "expression does not match any case"
  | _ -> fail_elab_variant exp.at "expression matches multiple cases"

(* Paths *)

and elab_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path) :
    (Il.Ast.path * plaintyp) attempt =
  let* path_il, plaintyp = elab_path' ctx plaintyp_expect path.it in
  let plaintyp = plaintyp $ plaintyp_expect.at in
  let typ_il = elab_plaintyp ctx plaintyp in
  Ok (path_il $$ (path.at, typ_il.it), plaintyp)

and elab_path' (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path') :
    (Il.Ast.path' * plaintyp') attempt =
  match path with
  | RootP -> elab_root_path plaintyp_expect
  | IdxP (path, exp) -> elab_idx_path ctx plaintyp_expect path exp
  | SliceP (path, exp_l, exp_h) ->
      elab_slice_path ctx plaintyp_expect path exp_l exp_h
  | DotP (path, atom) -> elab_dot_path ctx plaintyp_expect path atom

(* Root paths *)

and elab_root_path (plaintyp_expect : plaintyp) :
    (Il.Ast.path' * plaintyp') attempt =
  Ok (Il.Ast.RootP, plaintyp_expect.it)

(* Index paths *)

and elab_idx_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (exp : exp) : (Il.Ast.path' * plaintyp') attempt =
  let* path_il, plaintyp = elab_path ctx plaintyp_expect path in
  let* exp_il = elab_exp ctx (NumT `NatT $ exp.at) exp in
  let path_il = Il.Ast.IdxP (path_il, exp_il) in
  let* plaintyp = elab_as_list ctx plaintyp in
  Ok (path_il, plaintyp.it)

(* Slice paths *)

and elab_slice_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (exp_l : exp) (exp_h : exp) : (Il.Ast.path' * plaintyp') attempt =
  let* path_il, plaintyp = elab_path ctx plaintyp_expect path in
  let* exp_il_l = elab_exp ctx (NumT `NatT $ exp_l.at) exp_l in
  let* exp_il_h = elab_exp ctx (NumT `NatT $ exp_h.at) exp_h in
  let path_il = Il.Ast.SliceP (path_il, exp_il_l, exp_il_h) in
  let* _ = elab_as_list ctx plaintyp in
  Ok (path_il, plaintyp.it)

(* Dot paths *)

and elab_dot_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (atom : atom) : (Il.Ast.path' * plaintyp') attempt =
  let* path_il, plaintyp = elab_path ctx plaintyp_expect path in
  let* typfields = elab_as_struct ctx plaintyp in
  let* plaintyp =
    List.find_opt (fun (atom_t, _, _) -> atom.it = atom_t.it) typfields
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, plaintyp, _) -> Ok plaintyp
    | None -> fail atom.at "cannot infer type of field"
  in
  Ok (Il.Ast.DotP (path_il, atom), plaintyp.it)

(* Parameters *)

and elab_param (ctx : Ctx.t) (param : param) : Il.Ast.param =
  match param.it with
  | ExpP plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.ExpP typ_il $ param.at
  | DefP (id, tparams, params, plaintyp) ->
      check
        (List.map it tparams |> distinct ( = ))
        id.at "type parameters are not distinct";
      let params_il = List.map (elab_param ctx) params in
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.DefP (id, tparams, params_il, typ_il) $ param.at

(* Arguments *)

and elab_arg (ctx : Ctx.t) (param : param) (arg : arg) : Il.Ast.arg =
  match (param.it, arg.it) with
  | ExpP plaintyp, ExpA exp ->
      let+ exp_il = elab_exp ctx plaintyp exp in
      Il.Ast.ExpA exp_il $ arg.at
  | DefP (id_p, _, _, _), DefA id_a ->
      check (id_p.it = id_a.it) arg.at "argument does not match parameter";
      Il.Ast.DefA id_a $ arg.at
  | _ -> error arg.at "argument does not match parameter"

(* Premises *)

and elab_prem (ctx : Ctx.t) (prem : prem) : Ctx.t * Il.Ast.prem option =
  let ctx, prem_il_opt = elab_prem' ctx prem.it in
  let prem_il_opt = Option.map (fun prem_il -> prem_il $ prem.at) prem_il_opt in
  (ctx, prem_il_opt)

and elab_prem' (ctx : Ctx.t) (prem : prem') : Ctx.t * Il.Ast.prem' option =
  let wrap_ctx prem = (ctx, prem) in
  let wrap_some (ctx, prem) = (ctx, Some prem) in
  let wrap_none ctx = (ctx, None) in
  match prem with
  | VarPr (id, plaintyp) -> elab_var_prem ctx id plaintyp |> wrap_none
  | RulePr (id, exp) -> elab_rule_prem ctx id exp |> wrap_some
  | IfPr exp -> elab_if_prem ctx exp |> wrap_some
  | ElsePr -> elab_else_prem () |> wrap_ctx |> wrap_some
  | _ ->
      let+ _ = todo "elab_prem" (El.Print.string_of_prem (prem $ no_region)) in
      assert false

and elab_prems (ctx : Ctx.t) (prems : prem list) : Ctx.t * Il.Ast.prem list =
  List.fold_left
    (fun (ctx, prems_il) prem ->
      let ctx, prem_il_opt = elab_prem ctx prem in
      match prem_il_opt with
      | Some prem_il -> (ctx, prems_il @ [ prem_il ])
      | None -> (ctx, prems_il))
    (ctx, []) prems

(* Variable premises *)

and elab_var_prem (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id plaintyp

(* Rule premises *)

and elab_rule_prem (ctx : Ctx.t) (id : id) (exp : exp) : Ctx.t * Il.Ast.prem' =
  let nottyp, inputs = Ctx.find_rel ctx id in
  let+ notexp_il = elab_exp_not ctx nottyp exp in
  let exps_input, exps_output =
    let exps = notexp_il |> snd in
    List.mapi (fun idx exp -> (idx, exp)) exps
    |> List.partition (fun (idx, _) -> List.mem idx inputs)
    |> fun (exps_input, exps_output) ->
    (List.map snd exps_input, List.map snd exps_output)
  in
  let+ binds_input = Bind.binding_exps ctx.venv exps_input in
  if not (Envs.Bound.is_empty binds_input) then
    error exp.at
      (Format.asprintf "rule input has free variable(s): %s"
         (binds_input |> Envs.Bound.elements |> List.map it
        |> String.concat ", "));
  let+ binds_output = Bind.binding_exps ctx.venv exps_output in
  let ctx_local = binds_output |> Envs.Bound.elements |> Ctx.add_vars ctx in
  let prem_il = Il.Ast.RulePr (id, notexp_il) in
  (ctx_local, prem_il)

(* If premises :

   disambiguate `=` of whether it means equality or assignment,
   via dataflow analysis of ordered premises *)

and elab_if_eq_prem (ctx : Ctx.t) (at : region) (optyp : Il.Ast.optyp)
    (exp_il_l : Il.Ast.exp) (exp_il_r : Il.Ast.exp) : Ctx.t * Il.Ast.prem' =
  let+ kind = Bind.binding_if_prem ctx.venv at exp_il_l exp_il_r in
  match kind with
  | `Equality ->
      let exp_il =
        Il.Ast.CmpE (`EqOp, optyp, exp_il_l, exp_il_r) $$ (at, Il.Ast.BoolT)
      in
      let prem_il = Il.Ast.IfPr exp_il in
      (ctx, prem_il)
  | `AssignL binds ->
      let ctx = binds |> Envs.Bound.elements |> Ctx.add_vars ctx in
      let prem_il = Il.Ast.LetPr (exp_il_l, exp_il_r) in
      (ctx, prem_il)
  | `AssignR binds ->
      let ctx = binds |> Envs.Bound.elements |> Ctx.add_vars ctx in
      let prem_il = Il.Ast.LetPr (exp_il_r, exp_il_l) in
      (ctx, prem_il)

and elab_if_cond_prem (ctx : Ctx.t) (exp_il : Il.Ast.exp) : Il.Ast.prem' =
  let+ binds = Bind.binding_exp ctx.venv exp_il in
  if not (Envs.Bound.is_empty binds) then
    error exp_il.at
      (Format.asprintf "condition has free variable(s): %s"
         (binds |> Envs.Bound.elements |> List.map it |> String.concat ", "));
  Il.Ast.IfPr exp_il

and elab_if_prem (ctx : Ctx.t) (exp : exp) : Ctx.t * Il.Ast.prem' =
  let+ exp_il = elab_exp ctx (BoolT $ exp.at) exp in
  match exp_il.it with
  | CmpE (`EqOp, optyp, exp_il_l, exp_il_r) ->
      elab_if_eq_prem ctx exp_il.at optyp exp_il_l exp_il_r
  | _ ->
      let prem_il = elab_if_cond_prem ctx exp_il in
      (ctx, prem_il)

(* Else premises *)

and elab_else_prem () : Il.Ast.prem' = Il.Ast.ElsePr

(* Definitions *)

let rec elab_def (ctx : Ctx.t) (def : def) : Ctx.t * Il.Ast.def option =
  let wrap_some (ctx, def) = (ctx, Some def) in
  let wrap_none ctx = (ctx, None) in
  match def.it with
  | SynD (id, tparams) -> elab_syn_def ctx id tparams |> wrap_none
  | TypD (id, tparams, deftyp, _hints) ->
      elab_typ_def ctx id tparams deftyp |> wrap_some
  | VarD (id, plaintyp, _hints) -> elab_var_def ctx id plaintyp |> wrap_none
  | RelD (id, nottyp, hints) ->
      elab_rel_def ctx def.at id nottyp hints |> wrap_some
  | RuleD (id_rel, id_rule, exp, prems) ->
      elab_rule_def ctx def.at id_rel id_rule exp prems |> wrap_none
  | DecD (id, tparams, params, plaintyp, _hints) ->
      elab_dec_def ctx def.at id tparams params plaintyp |> wrap_some
  | DefD (id, targs, args, exp, prems) ->
      elab_def_def ctx def.at id targs args exp prems |> wrap_none
  | SepD -> ctx |> wrap_none

and elab_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Il.Ast.def list =
  List.fold_left
    (fun (ctx, defs_il) def ->
      let ctx, def_il_opt = elab_def ctx def in
      match def_il_opt with
      | Some def_il -> (ctx, defs_il @ [ def_il ])
      | None -> (ctx, defs_il))
    (ctx, []) defs

(* Type declarations *)

and elab_syn_def (ctx : Ctx.t) (id : id) (tparams : tparam list) : Ctx.t =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  check (valid_tid id) id.at "invalid type identifier";
  let td = TypeDef.Defining tparams in
  let ctx = Ctx.add_typdef ctx id td in
  if tparams = [] then
    let plaintyp = VarT (id, []) $ id.at in
    Ctx.add_metavar ctx id plaintyp
  else ctx

(* Type definitions *)

and elab_typ_def (ctx : Ctx.t) (id : id) (tparams : tparam list)
    (deftyp : deftyp) : Ctx.t * Il.Ast.def =
  let td_opt = Ctx.find_typdef_opt ctx id in
  let ctx =
    match td_opt with
    | Some (TypeDef.Defining tparams_defining) ->
        let tparams = List.map it tparams in
        let tparams_defining = List.map it tparams_defining in
        check
          (List.length tparams = List.length tparams_defining
          && List.for_all2 ( = ) tparams tparams_defining)
          id.at "type parameters do not match";
        ctx
    | None ->
        check (valid_tid id) id.at "invalid type identifier";
        let td = TypeDef.Defining tparams in
        let ctx = Ctx.add_typdef ctx id td in
        if tparams = [] then
          let plaintyp = VarT (id, []) $ id.at in
          Ctx.add_metavar ctx id plaintyp
        else ctx
    | _ -> error id.at "type was already defined"
  in
  check (List.for_all valid_tid tparams) id.at "invalid type parameter";
  let ctx_local = Ctx.add_tparams ctx tparams in
  let deftyp_il = elab_deftyp ctx_local deftyp in
  let def_il = Il.Ast.TypD (id, tparams, deftyp_il) $ deftyp.at in
  let td = TypeDef.Defined (tparams, deftyp) in
  let ctx = Ctx.update_typdef ctx id td in
  (ctx, def_il)

(* Variable declarations *)

and elab_var_def (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id plaintyp

(* Relation declarations *)

and fetch_rel_input_hint' (len : int) (hintexp : exp) : int list option =
  match hintexp.it with
  | SeqE exps ->
      List.fold_left
        (fun inputs exp ->
          match inputs with
          | Some inputs -> (
              match exp.it with
              | HoleE (`Num input) when input < len -> Some (inputs @ [ input ])
              | _ -> None)
          | None -> None)
        (Some []) exps
  | _ -> None

and fetch_rel_input_hint (at : region) (nottyp_il : Il.Ast.nottyp)
    (hints : hint list) : int list =
  let len = nottyp_il.it |> snd |> List.length in
  let hint_input_default = List.init len Fun.id in
  let hint_input =
    List.find_map
      (fun hint -> if hint.hintid.it = "input" then Some hint.hintexp else None)
      hints
  in
  match hint_input with
  | Some hintexp -> (
      let inputs_opt = fetch_rel_input_hint' len hintexp in
      match inputs_opt with
      | Some inputs -> inputs
      | None ->
          warn at
            (Format.asprintf
               "malformed input hint: should be a sequence of indexed holes \
                %%N (N < %d)"
               len);
          hint_input_default)
  (* If no hint is provided, assume all fields are inputs *)
  | None ->
      warn at "no input hint provided";
      hint_input_default

and elab_rel_def (ctx : Ctx.t) (at : region) (id : id) (nottyp : nottyp)
    (hints : hint list) : Ctx.t * Il.Ast.def =
  let nottyp_il = elab_nottyp ctx nottyp in
  let inputs = fetch_rel_input_hint at nottyp_il hints in
  let ctx = Ctx.add_rel ctx id nottyp inputs in
  let def_il = Il.Ast.RelD (id, nottyp_il, []) $ at in
  (ctx, def_il)

(* Rule definitions *)

and elab_rule_def (ctx : Ctx.t) (at : region) (id_rel : id) (id_rule : id)
    (exp : exp) (prems : prem list) : Ctx.t =
  let nottyp, inputs = Ctx.find_rel ctx id_rel in
  let ctx_local = ctx in
  let+ notexp_il = elab_exp_not ctx_local nottyp exp in
  let exps_input, exps_output =
    let exps = notexp_il |> snd in
    List.mapi (fun idx exp -> (idx, exp)) exps
    |> List.partition (fun (idx, _) -> List.mem idx inputs)
    |> fun (exps_input, exps_output) ->
    (List.map snd exps_input, List.map snd exps_output)
  in
  let+ binds_input = Bind.binding_exps ctx_local.venv exps_input in
  let ctx_local =
    binds_input |> Envs.Bound.elements |> Ctx.add_vars ctx_local
  in
  let ctx_local, prems_il = elab_prems ctx_local prems in
  let+ binds_output = Bind.binding_exps ctx_local.venv exps_output in
  if not (Envs.Bound.is_empty binds_output) then
    error exp.at
      (Format.asprintf "rule output has free variable(s): %s"
         (binds_output |> Envs.Bound.elements |> List.map it
        |> String.concat ", "));
  let rule = (id_rule, notexp_il, prems_il) $ at in
  Ctx.add_rule ctx id_rel rule

(* Function declarations *)

and elab_dec_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : Ctx.t * Il.Ast.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  let params_il = List.map (elab_param ctx) params in
  let typ_il = elab_plaintyp ctx plaintyp in
  let def_il = Il.Ast.DecD (id, tparams, params_il, typ_il, []) $ at in
  let ctx = Ctx.add_dec ctx id tparams params plaintyp in
  (ctx, def_il)

(* Function definitions *)

and elab_def_def (ctx : Ctx.t) (at : region) (id : id) (targs : plaintyp list)
    (args : arg list) (exp : exp) (prems : prem list) : Ctx.t =
  let tparams, params, plaintyp = Ctx.find_dec ctx id in
  check
    (List.length targs = List.length tparams)
    at "type arguments do not match";
  check (List.length params = List.length args) at "arguments do not match";
  let ctx_local = ctx in
  let args_il = List.map2 (elab_arg ctx_local) params args in
  let+ binds_input = Bind.binding_args ctx_local.venv args_il in
  let ctx_local =
    binds_input |> Envs.Bound.elements |> Ctx.add_vars ctx_local
  in
  let ctx_local, prems_il = elab_prems ctx_local prems in
  let+ exp_il = elab_exp ctx_local plaintyp exp in
  let+ binds_output = Bind.binding_exp ctx_local.venv exp_il in
  if not (Envs.Bound.is_empty binds_output) then
    error exp_il.at
      (Format.asprintf "output expression has free variable(s): %s"
         (binds_output |> Envs.Bound.elements |> List.map it
        |> String.concat ", "));
  let clause = (args_il, exp_il, prems_il) $ at in
  Ctx.add_clause ctx id clause

(* Spec *)

(* Populate rules to their respective relations *)

let populate_rule (ctx : Ctx.t) (def_il : Il.Ast.def) : Il.Ast.def =
  match def_il.it with
  | Il.Ast.RelD (id, nottyp_il, []) ->
      let rules_il = Ctx.find_rules ctx id in
      Il.Ast.RelD (id, nottyp_il, rules_il) $ def_il.at
  | Il.Ast.RelD _ -> error def_il.at "relation was already populated"
  | _ -> def_il

let populate_rules (ctx : Ctx.t) (spec_il : Il.Ast.spec) : Il.Ast.spec =
  List.map (populate_rule ctx) spec_il

(* Populate clauses to their respective function declarations *)

let populate_clause (ctx : Ctx.t) (def_il : Il.Ast.def) : Il.Ast.def =
  match def_il.it with
  | Il.Ast.DecD (id, tparams_il, params_il, typ_il, []) ->
      let clauses_il = Ctx.find_clauses ctx id in
      Il.Ast.DecD (id, tparams_il, params_il, typ_il, clauses_il) $ def_il.at
  | Il.Ast.DecD _ -> error def_il.at "declaration was already populated"
  | _ -> def_il

let populate_clauses (ctx : Ctx.t) (spec_il : Il.Ast.spec) : Il.Ast.spec =
  List.map (populate_clause ctx) spec_il

let elab_spec (spec : spec) : Il.Ast.spec =
  let ctx = Ctx.init () in
  let ctx, spec_il = elab_defs ctx spec in
  spec_il |> populate_rules ctx |> populate_clauses ctx
