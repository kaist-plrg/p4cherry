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

(* Dot output *)

let dot_of_func ((id_func, idx_clause, idx_arg) : func) : string =
  Il.Print.string_of_defid id_func
  ^ "/" ^ string_of_int idx_clause ^ "/" ^ string_of_int idx_arg
  |> String.escaped

let dot_of_rel ((id_rel, id_rule, idx_arg) : rel) : string =
  Il.Print.string_of_relid id_rel
  ^ "/"
  ^ Il.Print.string_of_ruleid id_rule
  ^ "/" ^ string_of_int idx_arg
  |> String.escaped

let dot_of_op (op : op) : string =
  (match op with
  | UnOp unop -> Il.Print.string_of_unop unop
  | BinOp binop -> Il.Print.string_of_binop binop
  | CmpOp cmpop -> Il.Print.string_of_cmpop cmpop
  | CatOp -> "cat"
  | MemOp -> "mem"
  | LenOp -> "len"
  | UpdOp -> "upd"
  | CastOp typ -> "cast" ^ Il.Print.string_of_typ typ
  | DownCastOp -> "downcast")
  |> String.escaped

let dot_of_label (label : label) : string =
  match label with
  | Inside -> "inside"
  | Rel rel -> dot_of_rel rel
  | Func func -> dot_of_func func
  | Op op -> dot_of_op op

let dot_of_edge (vid_from : vid) (label : label) (vid_to : vid) : string =
  Format.asprintf "  %d -> %d [label=\"%s\"];" vid_from vid_to
    (dot_of_label label)
