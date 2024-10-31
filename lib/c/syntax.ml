type bitwidth = BW8 | BW16 | BW32 | BW64

type ctyp =
  | CTVoid
  | CTChar
  | CTBool
  | CTInt
  | CTUInt
  | CTLInt
  | CTULInt
  (* | CTUIntBW of bitwidth *)
  (* | CTArray of ctyp *)
  | CTStruct of string
  | CTUnion of string
  | CTEnum of string
  | CTPointer of ctyp
  | CTArray of ctyp

and cvar = string

and bop =
  | CBEq
  | CBNe
  | CBLt
  | CBGt
  | CBGte
  | CBLte
  | CBAnd
  | CBOr
  | CBXor
  | CBLogicalAnd
  | CBLogicalOr
  | CBAdd
  | CBSub
  | CBMul
  | CBDiv
  | CBMod
  | CBShl
  | CBShr

and uop = CUNeg | CUAddressOf | CUDereference

and cexpr =
  | CEVar of cvar
  | CEBool of bool
  | CEInt of int
  | CECast of ctyp * cexpr
  | CEMember of cexpr * string
  | CECompExpr of bop * cexpr * cexpr
  | CEUniExpr of uop * cexpr
  | CECall of cexpr * cexpr list
  | CEStruct of cexpr list
  | CEArrayAccess of cexpr * cexpr

and cparam = ctyp * cvar

and cdecl =
  | CDVar of ctyp * cvar * cexpr option
  | CDStruct of string * (ctyp * string) list
  | CDUnion of string * (ctyp * string) list
  | CDEnum of string * string list
  | CDFunction of ctyp * string * cparam list * cblk
  | CDArray of ctyp * cvar * cexpr

and cstmt =
  | CSSkip
  | CSAssign of cexpr * cexpr
  | CSIf of cexpr * cblk * cblk
  | CSLabel of string
  | CSGoto of string
  | CSReturn of cexpr option
  | CSDecl of cdecl
  | CSExpr of cexpr
  | CSBlock of cblk
  | CSWhile of cexpr * cblk
  | CSBreak
  | CSSwitch of cexpr * (cexpr * cblk) list * cblk option

and cblk = cstmt list
and cpreprocessor = string list
and cprog = CProgram of cpreprocessor * cdecl list
