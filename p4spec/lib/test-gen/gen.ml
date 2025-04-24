module F = Format

type item = {
  name : string;
  typ_str : string;
  val_str : string;
  def_str : string;
}

let type_list =
  [
    (* Primitive types *)
    { name = "string"; typ_str = "string"; val_str = "str"; def_str = "" };
    { name = "bool"; typ_str = "bool"; val_str = "false"; def_str = "" };
    (* Number types *)
    { name = "int"; typ_str = "int"; val_str = "1"; def_str = "" };
    { name = "fint4"; typ_str = "int<4>"; val_str = "4s1"; def_str = "" };
    { name = "fint8"; typ_str = "int<8>"; val_str = "8s2"; def_str = "" };
    { name = "fbit1"; typ_str = "bit<1>"; val_str = "1w1"; def_str = "" };
    { name = "fbit4"; typ_str = "bit<4>"; val_str = "4w3"; def_str = "" };
    { name = "fbit8"; typ_str = "bit<8>"; val_str = "8w1"; def_str = "" };
    { name = "vbit4"; typ_str = "varbit<4>"; val_str = "4v2"; def_str = "" };
    { name = "vbit8"; typ_str = "varbit<8>"; val_str = "8v3"; def_str = "" };
    (* 3. Defined types *)
    (* 3a. Alias types *)
    {
      name = "def_fbit4";
      typ_str = "Def";
      val_str = "";
      def_str = "typedef bit<4> Def;\n";
    };
    (* 3b. Data types *)
    {
      name = "new_fbit4";
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
      name = "senum_fbit4";
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
      name = "tuple_bit4";
      typ_str = "tuple<bit<4>>";
      val_str = "{ 0 }";
      def_str = "";
    };
    {
      name = "tuple_bit4_bool";
      typ_str = "tuple<bit<4>, bool>";
      val_str = "{ 0, false }";
      def_str = "";
    };
    {
      name = "stack_1_fbit4";
      typ_str = "Stack_inner[1]";
      val_str = "";
      def_str = "header Stack_inner {\n  bit<4> x; \n}\n";
    };
    {
      name = "stack_4_fbit4";
      typ_str = "Stack_inner[4]";
      val_str = "";
      def_str = "header Stack_inner {\n  bit<4> x; \n}\n";
    };
    {
      name = "struct_fbit4";
      typ_str = "Sbit4";
      val_str = "";
      def_str = "struct Sbit4 {\n  bit<4> x;\n}\n";
    };
    {
      name = "struct_fbit4_bool";
      typ_str = "Sbit4bool";
      val_str = "";
      def_str = "struct Sbit4bool {\n  bit<4> x;\n  bool y;\n}\n";
    };
    {
      name = "header_fbit4";
      typ_str = "Hbit4";
      val_str = "";
      def_str = "header Hbit4 {\n  bit<4> x;\n}\n";
    };
    {
      name = "header_fbit4_bool";
      typ_str = "Hbit4bool";
      val_str = "";
      def_str = "header Hbit4bool {\n  bit<4> x;\n  bool y;\n}\n";
    };
    {
      name = "union";
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
    {
      name = "extern";
      typ_str = "Extn";
      val_str = "";
      def_str = "extern Extn {\n  Extn();\n}\n";
    };
    {
      name = "parser";
      typ_str = "Prsr";
      val_str = "";
      def_str = "parser Prsr();\n";
    };
    {
      name = "control";
      typ_str = "Ctrl";
      val_str = "";
      def_str = "control Ctrl();\n";
    };
    {
      name = "package";
      typ_str = "Pkg";
      val_str = "";
      def_str = "package Pkg();\n";
    };
    (* 4. Synthesized types *)
    { name = "default"; typ_str = ""; val_str = "..."; def_str = "" };
    { name = "seq"; typ_str = ""; val_str = "{ 0 }"; def_str = "" };
    {
      name = "seq_int_fbit8";
      typ_str = "";
      val_str = "{ 0, 8w3 }";
      def_str = "";
    };
    {
      name = "seq_int_str";
      typ_str = "";
      val_str = "{ 0, \"1\" }";
      def_str = "";
    };
    {
      name = "seqdefault_int";
      typ_str = "";
      val_str = "{ 2, ... }";
      def_str = "";
    };
    {
      name = "seqdefault_bool";
      typ_str = "";
      val_str = "{ false, ... }";
      def_str = "";
    };
    {
      name = "record_int_bool";
      typ_str = "";
      val_str = "{ x = 2, y = false }";
      def_str = "";
    };
    {
      name = "record_bool_int";
      typ_str = "";
      val_str = "{ y = 2, x = false }";
      def_str = "";
    };
    {
      name = "recorddefault_bool";
      typ_str = "";
      val_str = "{ y = false, ... }";
      def_str = "";
    };
    {
      name = "recorddefault_int";
      typ_str = "";
      val_str = "{ x = 2, ... }";
      def_str = "";
    };
    {
      name = "recorddefault_str";
      typ_str = "";
      val_str = "{ x = \"5\", ... }";
      def_str = "";
    };
    {
      name = "recorddefault_mismatch";
      typ_str = "";
      val_str = "{ y = 2, ... }";
      def_str = "";
    };
    { name = "invalid"; typ_str = ""; val_str = "{#}"; def_str = "" };
    {
      name = "set";
      typ_str = "";
      val_str = "vset";
      def_str = "value_set<bit<4>>(4) vset;\n";
    };
  ]

let is_synthtyp typ = typ.typ_str = ""

let sub_impl (typ_from : item) (typ_to : item) : (string * string) option =
  let filename = F.sprintf "Sub_impl-%s-to-%s.p4" typ_from.name typ_to.name in
  let defs =
    if typ_from.name = typ_to.name then typ_from.def_str
    else F.sprintf "%s%s" typ_from.def_str typ_to.def_str
  in
  match (typ_from.name, typ_to.name) with
  | _, _ ->
      if is_synthtyp typ_to then Option.none
      else if typ_from.name = "set" then
        ( filename,
          F.sprintf
            "%svoid func(%s a){\n\
            \  return;\n\
             }\n\
             parser P() {\n\
            \  %s\n\
            \  state start {\n\
            \    func(%s);\n\
             transition accept;\n\
            \  }\n\
             }"
            typ_to.def_str typ_to.typ_str typ_from.def_str typ_from.val_str )
        |> Option.some
      else if is_synthtyp typ_from then
        ( filename,
          F.sprintf "%s\n%s func(){\n  return %s;\n}" defs typ_to.typ_str
            typ_from.val_str )
        |> Option.some
      else
        ( filename,
          F.sprintf "%s\n%s func(%s a){\n  return a;\n}" defs typ_to.typ_str
            typ_from.typ_str )
        |> Option.some

let sub_expl (typ_from : item) (typ_to : item) : (string * string) option =
  let filename = F.sprintf "Sub_expl-%s-to-%s.p4" typ_from.name typ_to.name in
  let defs =
    if typ_from.name = typ_to.name then typ_from.def_str
    else F.sprintf "%s%s" typ_from.def_str typ_to.def_str
  in
  match (typ_from.name, typ_to.name) with
  | _, _ ->
      if is_synthtyp typ_to then Option.none
      else if typ_from.name = "set" then
        ( filename,
          F.sprintf
            "%svoid func(%s a){\n\
            \  return;\n\
             }\n\
             parser P() {\n\
            \  %s\n\
            \  state start {\n\
            \    func((%s) %s);\n\
             transition accept;\n\
            \  }\n\
             }"
            typ_to.def_str typ_to.typ_str typ_from.def_str typ_to.typ_str
            typ_from.val_str )
        |> Option.some
      else if is_synthtyp typ_from then
        ( filename,
          F.sprintf "%s%s func(){\n  return (%s) %s;\n}" defs typ_to.typ_str
            typ_to.typ_str typ_from.val_str )
        |> Option.some
      else
        ( filename,
          F.sprintf "%s%s func(%s a)\n{\n  return (%s) a;\n}" defs
            typ_to.typ_str typ_from.typ_str typ_to.typ_str )
        |> Option.some

type item_template = {
  name : string;
  typ_str : string -> string;
  def_str : string -> string;
}

let empty_def _ : string = ""

let outer_list =
  [
    {
      name = "list";
      typ_str = (fun t -> F.sprintf "list<%s>" t);
      def_str = empty_def;
    };
    {
      name = "tuple";
      typ_str = (fun t -> F.sprintf "tuple<%s>" t);
      def_str = empty_def;
    };
    {
      name = "senum";
      typ_str = (fun _ -> "Senum_Out");
      def_str = (fun t -> F.sprintf "enum %s Senum_Out;\n" t);
    };
    {
      name = "stack";
      typ_str = (fun t -> F.sprintf "%s[4]" t);
      def_str = empty_def;
    };
    {
      name = "struct";
      typ_str = (fun t -> F.sprintf "S_Out<%s>" t);
      def_str = (fun _ -> "struct S_Out<T> {\n  T x;\n}\n");
    };
    {
      name = "header";
      typ_str = (fun t -> F.sprintf "H_Out<%s>" t);
      def_str = (fun _ -> "header H_Out<T> {\n  T x;\n}\n");
    };
    {
      name = "union";
      typ_str = (fun t -> F.sprintf "Hu_Out<%s>" t);
      def_str = (fun _ -> "header_union Hu_Out<T> {\n  T x; \n}\n");
    };
    {
      name = "def";
      typ_str = (fun _ -> "Def_Out");
      def_str = (fun t -> F.sprintf "typedef %s Def_Out;\n" t);
    };
    {
      name = "new";
      typ_str = (fun _ -> "New");
      def_str = (fun t -> F.sprintf "type %s New_Out;\n" t);
    };
  ]

let type_wf_nesting (typ_outer : item_template) (typ_inner : item) :
    (string * string) option =
  let filename =
    F.sprintf "Type_wf-%s-in-%s.p4" typ_inner.name typ_outer.name
  in
  let defs = typ_inner.def_str ^ typ_outer.def_str typ_inner.typ_str in
  if is_synthtyp typ_inner then Option.none
  else
    match typ_outer.name with
    | "senum" ->
        if typ_inner.val_str = "" then Option.none
        else
          ( filename,
            F.sprintf
              "%senum %s Senum_Out { V = %s }\n\
               Senum_Out func(Senum_Out a){\n\
              \  return a;\n\
               }"
              defs typ_inner.typ_str typ_inner.val_str )
          |> Option.some
    | _ ->
        let typ_str = typ_outer.typ_str typ_inner.typ_str in
        ( filename,
          F.sprintf "%s%s func(%s a){\n  return a;\n}" defs typ_str typ_str )
        |> Option.some

let type_wf_double_nesting (typ_outer : item_template) (typ_mid : item_template)
    (typ_inner : item) : (string * string) option =
  let filename =
    F.sprintf "Type_wf-%s-in-%s-in-%s.p4" typ_inner.name typ_mid.name
      typ_outer.name
  in
  let defs =
    if typ_outer.name = typ_mid.name then
      typ_inner.def_str ^ typ_mid.def_str typ_inner.typ_str
    else
      typ_inner.def_str
      ^ typ_mid.def_str typ_inner.typ_str
      ^ typ_outer.def_str (typ_mid.typ_str typ_inner.typ_str)
  in
  if is_synthtyp typ_inner then Option.none
  else if typ_outer.name = "senum" || typ_mid.name = "senum" then Option.none
  else
    let typ_str = typ_outer.typ_str (typ_mid.typ_str typ_inner.typ_str) in
    (filename, F.sprintf "%s%s func(%s a){\n  return a;\n}" defs typ_str typ_str)
    |> Option.some

let sub : (string * string) list =
  let sub_impl =
    List.map
      (fun typ_from ->
        List.map
          (fun typ_to -> sub_impl typ_from typ_to |> Option.to_list)
          type_list
        |> List.flatten)
      type_list
    |> List.flatten
  in
  let sub_expl =
    List.map
      (fun typ_from ->
        List.map
          (fun typ_to -> sub_expl typ_from typ_to |> Option.to_list)
          type_list
        |> List.flatten)
      type_list
    |> List.flatten
  in
  sub_impl @ sub_expl

let type_wf : (string * string) list =
  let type_wf_nesting =
    List.concat_map
      (fun typ_inner ->
        List.concat_map
          (fun typ_outer ->
            type_wf_nesting typ_outer typ_inner |> Option.to_list)
          outer_list)
      type_list
  in
  let type_wf_double_nesting =
    List.concat_map
      (fun typ_outer ->
        List.concat_map
          (fun typ_mid ->
            List.concat_map
              (fun typ_inner ->
                type_wf_double_nesting typ_outer typ_mid typ_inner
                |> Option.to_list)
              type_list)
          outer_list)
      outer_list
  in
  type_wf_nesting @ type_wf_double_nesting

let generate : (string * string) list = sub @ type_wf
