open Domain.Dom
open Tdomain.Types

(* Environment for variable identifiers *)

module SType = MakeQuad (Type) (Dir) (Ctk) (MakeOption (Value))
module Frame = MakeIdEnv (SType)

(* Environment for type identifiers *)

module TDEnv = MakeTIdEnv (TypeDef)

(* Environment for function identifiers *)

module FDEnv = MakeFIdEnv (FuncDef)

(* Environment for constructor identifiers *)

module CDEnv = MakeCIdEnv (ConsDef)
