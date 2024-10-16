type ctyp =
  | CTVoid
  | CTChar
  | CTInt
  | CTUInt
  | CTLInt
  | CTULInt
  | CTArray of ctyp
  | CTStruct of string

type cvar = string

type bop = 
  | CBEq
  | CBNe
  | CBLt
  | CBGt
  | CBGte
  | CBLte
  | CBAnd  
  | CBOr
  | CBAdd 
  | CBSub 
  | CBMul 
  | CBDiv 
  | CBMod 
  
type uop = 
  | CUNeg

type cexpr =
  | CEVar of cvar
  | CEBool of bool
  | CEInt of int
  | CEMember of cexpr * string
  | CECompExpr of bop * cexpr * cexpr
  | CEUniExpr of uop * cexpr

type cparam = ctyp * string

and cdecl =
  | CDStruct of string * (ctyp * string) list
  | CDFunction of ctyp * string * cparam list * cblk

and cstmt =
  | CSDecl of cdecl
  | CSSkip
  | CSAssign of cexpr * cexpr
  | CSIf of cexpr * cblk * cblk
  | CSCall of cvar * cexpr list
  | CSLabel of string
  | CSGoto of string
  | CSReturn of cexpr option

and cblk = cstmt list
                 
type cprog =
  | CProgram of cdecl list 

