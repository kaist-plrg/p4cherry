(* Classification of premises *)

type t =
  | BoolK
  | VarK
  | UnK of [ `Not ]
  | BinK of [ `And | `Or | `Impl | `Equiv ]
  | CmpK of [ `Eq | `Ne | `Lt | `Gt | `Le | `Ge ]
  | SubK
  | MatchK
  | MemK
  | DotK
  | IdxK
  | CallK

(* Stringifier *)

let to_string (kind : t) : string =
  match kind with
  | BoolK -> "bool"
  | VarK -> "var"
  | UnK `Not -> "un-not"
  | BinK `And -> "bin-and"
  | BinK `Or -> "bin-or"
  | BinK `Impl -> "bin-impl"
  | BinK `Equiv -> "bin-equiv"
  | CmpK `Eq -> "cmp-eq"
  | CmpK `Ne -> "cmp-ne"
  | CmpK `Lt -> "cmp-lt"
  | CmpK `Gt -> "cmp-gt"
  | CmpK `Le -> "cmp-le"
  | CmpK `Ge -> "cmp-ge"
  | SubK -> "sub"
  | MatchK -> "match"
  | MemK -> "mem"
  | DotK -> "dot"
  | IdxK -> "idx"
  | CallK -> "call"

(* Comparison *)

let compare (kind_a : t) (kind_b : t) : int =
  let tag (kind : t) : int =
    match kind with
    | BoolK -> 0
    | VarK -> 1
    | UnK `Not -> 2
    | BinK `And -> 3
    | BinK `Or -> 4
    | BinK `Impl -> 5
    | BinK `Equiv -> 6
    | CmpK `Eq -> 7
    | CmpK `Ne -> 8
    | CmpK `Lt -> 9
    | CmpK `Gt -> 10
    | CmpK `Le -> 11
    | CmpK `Ge -> 12
    | SubK -> 13
    | MatchK -> 14
    | MemK -> 15
    | DotK -> 16
    | IdxK -> 17
    | CallK -> 18
  in
  Int.compare (tag kind_a) (tag kind_b)
