(* Contains the list of type declarations to be inserted at the beginning of the file *)
type t = Il.Ast.typ' list

let empty = []

let contains_typ_decl (t: Il.Ast.typ') (ts: t): bool =
  List.exists (fun t' -> t = t') ts

let add_typ_decl (t: Il.Ast.typ') (ts: t): t =
  if contains_typ_decl t ts then ts else t :: ts