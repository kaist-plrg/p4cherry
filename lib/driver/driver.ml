open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Cclos
open Runtime.Context
open Runtime.Signal

module type ARCH = sig
  val drive : program -> Stf.Ast.stmt list -> unit
  val interp_extern : Ctx.t -> Sig.t * Ctx.t
end

module type INST = sig
  val instantiate_from_cclos :
    CCEnv.t ->
    Sto.t ->
    ICtx.t ->
    Path.t ->
    CClos.t ->
    typ list ->
    arg list ->
    Sto.t

  val instantiate_program : program -> CCEnv.t * Sto.t * Ctx.t
end

module type INTERP = sig
  val init : Sto.t -> unit
  val interp_type : Ctx.t -> typ -> Type.t
  val interp_expr : Ctx.t -> expr -> Ctx.t * Value.t
  val interp_call : Ctx.t -> expr -> typ list -> arg list -> Sig.t * Ctx.t
end

module type DRIVER = sig
  val run : program -> Stf.Ast.stmt list -> unit
  val inst : program -> CCEnv.t * Sto.t * Ctx.t
end

module Make
    (MakeArch : functor (Inst : INST) (Interp : INTERP) -> ARCH)
    (MakeInst : functor (Interp : INTERP) -> INST)
    (MakeInterp : functor (Arch : ARCH) -> INTERP) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Inst) (Interp)
  and Inst : INST = MakeInst (Interp)
  and Interp : INTERP = MakeInterp (Arch)

  let run = Arch.drive
  let inst = Inst.instantiate_program
end
