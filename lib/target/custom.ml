open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Context
open Driver

module Make (Inst : INST) (Interp : INTERP) : ARCH = struct
  let init (ctx : Ctx.t) =
    (* Add "b" to the object environment *)
    let typ = Type.BitT (Bigint.of_int 32) in
    let value = Value.BitV (Bigint.of_int 32, Bigint.of_int 42) in
    Ctx.add_var_obj "b" typ value ctx

  let make_args (args : Var.t list) =
    List.map (fun arg -> ExprA (VarE (Bare arg))) args

  let drive_proto (ctx : Ctx.t) =
    let func = ExprAccE (ExprAccE (VarE (Bare "main"), "_p"), "apply") in
    let targs = [] in
    let args = make_args [ "b" ] in
    Interp.interp_call ctx func targs args |> snd

  let drive (program : program) (_stf : Stf.Ast.stmt list) =
    let _ccenv, sto, ctx = Inst.instantiate_program program in
    let ctx = init ctx in
    Interp.init sto;
    Format.printf "Initial context\n%a@\n" Ctx.pp ctx;
    let ctx = drive_proto ctx in
    Format.printf "After calling main._p\n%a@\n" Ctx.pp ctx;
    ()

  let interp_extern (_ctx : Ctx.t) = assert false
end
