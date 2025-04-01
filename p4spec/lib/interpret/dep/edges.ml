open Il.Ast

(* Label for categorizing dependency *)

type rel = id * id * int
type func = id * int * int

type op =
  | UnOp of unop
  | BinOp of binop
  | CmpOp of cmpop
  | CatOp
  | MemOp
  | LenOp
  | UpdOp
  | CastOp of typ
  | DownCastOp

type label = Inside | Rel of rel | Func of func | Op of op

(* Set of edges *)

module E = Hashtbl.Make (struct
  type t = label * vid

  let equal (label_a, vid_a) (label_b, vid_b) =
    label_a = label_b && vid_a = vid_b

  let hash (label, vid) = Hashtbl.hash (label, vid)
end)

type t = unit E.t
