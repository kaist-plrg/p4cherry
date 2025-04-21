module F = Format

type item = {
  name : string;
  typ_str : string;
  val_str : string;
  def_str : string;
}

let type_list =
  [
    { name = "string"; typ_str = "string"; val_str = "str"; def_str = "" };
    { name = "bool"; typ_str = "bool"; val_str = "false"; def_str = "" };
    (* Number types *)
    { name = "int"; typ_str = "int"; val_str = "1"; def_str = "" };
    { name = "fint4"; typ_str = "int<4>"; val_str = "4s1"; def_str = "" };
    { name = "fint8"; typ_str = "int<8>"; val_str = "8s2"; def_str = "" };
    { name = "fbit4"; typ_str = "bit<4>"; val_str = "4w3"; def_str = "" };
    { name = "fbit8"; typ_str = "bit<8>"; val_str = "8w1"; def_str = "" };
    { name = "vbit4"; typ_str = "varbit<4>"; val_str = "4v2"; def_str = "" };
    { name = "vbit8"; typ_str = "varbit<8>"; val_str = "8v3"; def_str = "" };
    (* 3. Defined types *)
    (* 3a. Alias types *)
    {
      name = "def";
      typ_str = "Def";
      val_str = "";
      def_str = "typedef bit<4> Def;\n";
    };
    (* 3b. Data types *)
    {
      name = "new";
      typ_str = "New";
      val_str = "";
      def_str = "type bit<4> New;\n";
    };
    {
      name = "enum";
      typ_str = "Enum";
      val_str = "";
      def_str = "enum Enum { Enum1, Enum2, Enum3 }\n";
    };
    {
      name = "senum";
      typ_str = "SEnum";
      val_str = "";
      def_str = "enum bit<4> SEnum { Enum1 = 1, Enum2 = 2, Enum3 = 3}\n";
    };
    {
      name = "list";
      typ_str = "list<bit<4>>";
      val_str = "{ 0, 1 }";
      def_str = "";
    };
    {
      name = "tuple";
      typ_str = "tuple<bit<4>>";
      val_str = "{ 0, \"1\"}";
      def_str = "";
    };
    {
      name = "headerstack";
      typ_str = "Stack_inner[4]";
      val_str = "";
      def_str = "header Stack_inner {\n  bit<4> x; \n}\n";
    };
    {
      name = "struct";
      typ_str = "S";
      val_str = "";
      def_str = "struct S {\n  bit<4> x; \n}\n";
    };
    {
      name = "header";
      typ_str = "H";
      val_str = "";
      def_str = "header H {\n  bit<4> x; \n}\n";
    };
    {
      name = "headerunion";
      typ_str = "Hu";
      val_str = "";
      def_str =
        "header Union_inner {\n\
        \ bit<4> x; \n\
         }\n\
         header_union Hu {\n\
        \  Union_inner x; \n\
         }\n";
    };
    (* 3c. Object types *)
    (* 4. Synthesized types *)
    { name = "default"; typ_str = ""; val_str = "..."; def_str = "" };
    { name = "seq"; typ_str = ""; val_str = "{ 0 }"; def_str = "" };
    { name = "seqdefault"; typ_str = ""; val_str = "{ 2, ... }"; def_str = "" };
    { name = "record"; typ_str = ""; val_str = "{ b = 2, f = 3 }"; def_str = "" };
    { name = "recorddefault"; typ_str = ""; val_str = "{ b = 2, ... }"; def_str = "" };
    { name = "invalid"; typ_str = ""; val_str = "{#}"; def_str = "" };
  ]

let contents_sub_impl (typ_from : item) (typ_to : item) : string =
  let defs =
    if typ_from.name = typ_to.name then typ_from.def_str
    else F.sprintf "%s%s" typ_from.def_str typ_to.def_str
  in
  match (typ_from.name, typ_to.name) with
  | "seq", _ ->
      F.sprintf "%s\n%s func(){\n  return %s;\n}" defs typ_to.typ_str
        typ_from.val_str
  | _, "seq" ->
      (* Doesn't work for now*)
      F.sprintf "%s%sbool func(out %s a){\n  %s  return a;\n}" typ_from.def_str
        typ_to.def_str typ_from.typ_str typ_to.val_str
  | _, _ ->
      F.sprintf "%s\n%s func(%s a)\n{\n  return a;\n}" defs typ_to.typ_str
        typ_from.typ_str

let contents_sub_expl (typ_from : item) (typ_to : item) : string =
  match (typ_from.name, typ_to.name) with
  | _, _ ->
      (* Select a template based on the to/from types *)
      F.sprintf "%s func(%s a)\n{\n  return (%s) a;\n}" typ_to.typ_str
        typ_from.typ_str typ_to.typ_str

(* Generate filename string*)
let filename_sub_impl (typ_from : item) (typ_to : item) : string =
  F.sprintf "impl-%s-to-%s.p4" typ_from.name typ_to.name

let filename_sub_expl (typ_from : item) (typ_to : item) : string =
  F.sprintf "expl-%s-to-%s.p4" typ_from.name typ_to.name

let sub : (string * string) list =
  let sub_impl =
    List.map
      (fun typ_from ->
        List.map
          (fun typ_to ->
            ( filename_sub_impl typ_from typ_to,
              contents_sub_impl typ_from typ_to ))
          type_list)
      type_list
    |> List.flatten
  in
  let sub_expl =
    List.map
      (fun typ_from ->
        List.map
          (fun typ_to ->
            ( filename_sub_expl typ_from typ_to,
              contents_sub_expl typ_from typ_to ))
          type_list)
      type_list
    |> List.flatten
  in
  sub_impl @ sub_expl
