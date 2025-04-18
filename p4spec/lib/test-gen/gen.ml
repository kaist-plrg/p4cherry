module F = Format

type item = {
  name: string;
  typ_str: string;
  val_str: string;
}

let type_list = [
  { name = "int"; typ_str = "int"; val_str = "1" };
  { name = "fint4"; typ_str = "int<4>"; val_str = "4s1" };
  { name = "fint8"; typ_str = "int<8>"; val_str = "8s2" };
  { name = "fbit4"; typ_str = "bit<4>"; val_str = "4w3" };
  { name = "fbit8"; typ_str = "bit<8>"; val_str = "8w1" };
  { name = "vbit4"; typ_str = "varbit<4>"; val_str = "4v2" };
  { name = "vbit8"; typ_str = "varbit<8>"; val_str = "8v3" };
  { name = "bool"; typ_str = "bool"; val_str = "false" };
  { name = "string"; typ_str = "string"; val_str = "str" };
  { name = "seq"; typ_str = ""; val_str = "{ 0 };" };
  { name = "header"; typ_str = "H"; val_str = "header H {\n  bit<8> x; \n}\n" };
  { name = "struct"; typ_str = "S"; val_str = "struct S {\n  bit<8> x; \n}\n" };
  { name = "tuple"; typ_str = ""; val_str = "{\n  0, \"1\"}" };
]

let contents_sub_expl (typ_from : item) (typ_to : item) : string =
  match (typ_from.name, typ_to.name) with
  | _, _ ->
    (* Select a template based on the to/from types *)
    F.sprintf "%s func(%s a)\n{\n  return %sa;\n}"
      typ_to.typ_str typ_from.typ_str ""

(* Generated filename string*)
let filename_sub_expl (typ_from : item) (typ_to : item) : string =
  F.sprintf "expl-%s-to-%s.p4" typ_from.name typ_to.name

let sub_expl : (string * string) list =
  List.map (fun typ_from -> List.map (fun typ_to ->
    (
      filename_sub_expl typ_from typ_to,
      contents_sub_expl typ_from typ_to
    )) type_list
    ) type_list |> List.flatten
