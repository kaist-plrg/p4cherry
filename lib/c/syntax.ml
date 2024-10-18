type bitwidth = BW8 | BW16 | BW32 | BW64

type ctyp =
  | CTVoid
  | CTChar
  | CTInt
  | CTUInt
  | CTLInt
  | CTULInt
  | CTUIntBW of bitwidth
  | CTArray of ctyp
  | CTStruct of string
  | CTPointer of ctyp

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
  | CUReference
  | CUDereference

type cexpr =
  | CEVar of cvar
  | CEBool of bool
  | CEInt of int
  | CEMember of cexpr * string
  | CECompExpr of bop * cexpr * cexpr
  | CEUniExpr of uop * cexpr
  | CECall of cvar * cexpr list

type cparam = ctyp * cvar

and cdecl =
  | CDVar of ctyp * cvar
  | CDStruct of string * (ctyp * string) list
  | CDFunction of ctyp * string * cparam list * cblk
  | CDEmpty (* Not an actual C type. Hack for compiler to print empty code for unimplemented components *)

and cstmt =
  | CSSkip
  | CSAssign of cexpr * cexpr
  | CSIf of cexpr * cblk * cblk
  | CSLabel of string
  | CSGoto of string
  | CSReturn of cexpr option
  | CSDecl of cdecl
  | CSExpr of cexpr

and cblk = cstmt list

type cpreprocessor = string list
                 
type cprog = CProgram of cpreprocessor * cdecl list