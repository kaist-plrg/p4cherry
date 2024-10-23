module F = Format
open Syntax

let rec pp_ctyp ppf (typ: ctyp) =
  match typ with
  | CTVoid -> F.fprintf ppf "void"
  | CTChar -> F.fprintf ppf "char"
  | CTBool -> F.fprintf ppf "bool"
  | CTInt -> F.fprintf ppf "int"
  | CTUInt -> F.fprintf ppf "unsigned int"
  | CTLInt -> F.fprintf ppf "long int"
  | CTULInt -> F.fprintf ppf "unsigned long int"
  (* | CTUIntBW bw -> F.fprintf ppf "uint%d_t" (match bw with
    | BW8 -> 8
    | BW16 -> 16
    | BW32 -> 32
    | BW64 -> 64
  ) *)
  | CTArray typ -> F.fprintf ppf "%a[]" pp_ctyp typ
  | CTStruct name -> F.fprintf ppf "struct %s" name
  | CTPointer typ -> F.fprintf ppf "%a*" pp_ctyp typ

and pp_bop ppf (op: bop) =
  F.fprintf ppf "%s" (match op with
    | CBEq -> "=="
    | CBNe -> "!="
    | CBLt -> "<"
    | CBGt -> ">"
    | CBGte -> ">="
    | CBLte -> "<="
    | CBAnd -> "&&"
    | CBOr -> "||"
    | CBAdd -> "+"
    | CBSub -> "-"
    | CBMul -> "*"
    | CBDiv -> "/"
    | CBMod -> "%"
  )

and pp_uop ppf (op: uop) =
  F.fprintf ppf "%s" (match op with
    | CUNeg -> "-"
    | CUReference -> "&"
    | CUDereference -> "*"
  )

and pp_kv ppf (typ, field) = 
  F.fprintf ppf "%a %s;" pp_ctyp typ field

and pp_kv_list ppf (kv_list: (ctyp * string) list) = 
  F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf "@ ") pp_kv ppf kv_list

and pp_struct ppf (name, kv_list: string * (ctyp * string) list) =
  F.fprintf ppf 
  "struct %s {@;<1 2>\
  @[<v 0>%a@]\
  @ };\
  "
  name pp_kv_list kv_list

and pp_param ppf (typ, name: ctyp * string) =
  F.fprintf ppf "%a %s" pp_ctyp typ name

and pp_params ppf (param_list: cparam list) =
  F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf ",@ ") pp_param ppf param_list

and pp_expr ppf (expr: cexpr) =
  match expr with
  | CEVar var -> F.fprintf ppf "%s" var
  | CEBool b -> F.fprintf ppf "%b" b
  | CEInt i -> F.fprintf ppf "%d" i
  | CEMember (expr, field) -> F.fprintf ppf "%a.%s" pp_expr expr field
  | CECompExpr (op, lhs, rhs) -> F.fprintf ppf "%a %a %a" pp_expr lhs pp_bop op pp_expr rhs
  | CEUniExpr (op, expr) -> F.fprintf ppf "(%a%a)" pp_uop op pp_expr expr
  | CECall (name, arg_list) -> F.fprintf ppf "%s(%a)" name pp_arg_list arg_list

and pp_arg_list ppf (arg_list: cexpr list) = 
  F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf ", ") pp_expr ppf arg_list

and pp_stmt ppf (stmt: cstmt) = 
  match stmt with
  | CSDecl decl -> pp_decl ppf decl
  | CSSkip -> F.fprintf ppf ";"
  | CSAssign (lhs, rhs) -> F.fprintf ppf "%a = %a;" pp_expr lhs pp_expr rhs
  | CSIf (cond, then_blk, else_blk) -> F.fprintf ppf "if (%a) %a else %a"
    pp_expr cond pp_block then_blk pp_block else_blk
  | CSLabel label -> F.fprintf ppf "%s:" label
  | CSGoto label -> F.fprintf ppf "goto %s;" label
  | CSReturn None -> F.fprintf ppf "return;"
  | CSReturn (Some expr) -> F.fprintf ppf "return %a;" pp_expr expr
  | CSExpr expr -> F.fprintf ppf "%a;" pp_expr expr

and pp_stmt_list ppf (stmt_list: cstmt list) = 
  F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf "@ ") pp_stmt ppf stmt_list
and pp_block ppf (stmt_list: cblk) = 
  F.fprintf ppf 
  "{@;<1 2>\
  @[<v 0>%a@]\
  @ }\
  " pp_stmt_list stmt_list

and pp_func ppf (ret_type, name, param_list, block: ctyp * string * cparam list * cblk) = 
  F.fprintf ppf "%a %s(@[<hv 0>%a@]) %a" pp_ctyp ret_type name pp_params param_list pp_block block

and pp_decl ppf (decl: cdecl) =
  match decl with
  | CDVar (typ, name) -> F.fprintf ppf "%a %s;" pp_ctyp typ name
  | CDStruct (name, kv_list)-> pp_struct ppf (name, kv_list)
  | CDFunction (ret_type, name, param_list, block) -> pp_func ppf (ret_type, name, param_list, block)

and pp_decls ppf (decls: cdecl list) = 
  (F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf "@,@,") pp_decl) ppf decls

and pp_preprocessor ppf (directives: string list) = 
  F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf "@,") F.pp_print_string ppf directives

and pp_program ppf (prog: cprog) = 
  let CProgram (preprocessor, decl_list) = prog in
  F.fprintf ppf "@[<v 0>%a@,@,%a@]@." pp_preprocessor preprocessor pp_decls decl_list

(* Inline Exploratory Testing *)

(* #include <cluj_core.h>

struct metadata { char ok; } metadata;

struct headers { } headers;

void MyParser (void* packet, struct headers* hdr, struct metadata* meta, struct standard_metadata_t* standard_metadata) {
  start:
    goto accept;

  accept:
  reject:
    return;
}
void MyVerifyChecksum (struct headers* hdr, struct metadata* meta) { }
void MyIngress (struct headers* hdr, struct metadata* meta, struct standard_metadata_t* standard_metadata)
{
  (\*standard_metadata).egress_spec = 2;
}
void MyEgress (struct headers* hdr,struct metadata* meta, struct standard_metadata_t* standard_metadata)
{ }
void MyUpdateChecksum (struct headers* hdr, struct metadata* meta) { }
void MyDeparser (struct headers* hdr) { }
int main () {
  uint16_t pkt_address;
  pkt_address = get_packet_addr();
  struct headers h;
  struct metadata m;  
  struct standard_metadata_t sm;
  MyParser(pkt_address, &hdr, &m, &sm);
  MyVerifyChecksum(&h, &m);
  MyIngress(&h, &m, &sm);
  MyEgress(&h, &m, &sm);
  MyUpdateChecksum(&h, &m);
  MyDeparser(&h);
}*)

let sample_program = CProgram ( [ "#include <cluj_core.h>"
  ; "#include <cluj.h>"
  ; "#include <endian.h>"
  ; "#include <stdbool.h>"
  ; "#include <stdint.h>"
  ; "#include <stdlib.h>"
  ; "#include <unistd.h>"],
  [ CDStruct ("metadata", [(CTChar, "ok")])
  ; CDStruct ("headers", [])
  ; CDFunction (CTVoid, "MyParser", 
    [ (CTPointer (CTStruct "headers"), "hdr")
    ; (CTPointer (CTStruct "metadata"), "meta")
    ; (CTPointer (CTStruct "standard_metadata_t"), "standard_metadata")
    ], 
    [ CSLabel "start"
    ; CSGoto "accept"
    ; CSLabel "accept"
    ; CSLabel "reject"
    ; CSReturn None
    ])
  ; CDFunction (CTVoid, "MyVerifyChecksum", 
    [ (CTPointer (CTStruct "headers"), "hdr")
    ; (CTPointer (CTStruct "metadata"), "meta")
    ], 
    [ CSReturn None
    ])
  ; CDFunction (CTVoid, "MyIngress", 
    [ (CTPointer (CTStruct "headers"), "hdr")
    ; (CTPointer (CTStruct "metadata"), "meta")
    ; (CTPointer (CTStruct "standard_metadata_t"), "standard_metadata")
    ], 
    [ CSAssign (CEMember (CEUniExpr (CUDereference, CEVar "standard_metadata"), "egress_spec"), CEInt 2)
    ])
  ; CDFunction (CTVoid, "MyEgress",
    [ (CTPointer (CTStruct "headers"), "hdr")
    ; (CTPointer (CTStruct "metadata"), "meta")
    ; (CTPointer (CTStruct "standard_metadata_t"), "standard_metadata")
    ], 
    [ CSReturn None
    ])
  ; CDFunction (CTVoid, "MyUpdateChecksum",
    [ (CTPointer (CTStruct "headers"), "hdr")
    ; (CTPointer (CTStruct "metadata"), "meta")
    ], 
    [ CSReturn None
    ])
  ; CDFunction (CTVoid, "MyDeparser",
    [ (CTPointer (CTStruct "headers"), "hdr")
    ], 
    [ CSReturn None
    ])
  ; CDFunction (CTInt, "main", [], 
    [ CSAssign (CEVar "pkt_address", CECall ("get_packet_addr", []))
    ; CSDecl (CDVar (CTStruct "headers", "h"))
    ; CSDecl (CDVar (CTStruct "metadata", "m"))
    ; CSDecl (CDVar (CTStruct "standard_metadata_t", "sm"))
    ; CSExpr (CECall ("MyParser", [
        CEVar "pkt_address"; 
        CEUniExpr (CUReference, CEVar "h"); 
        CEUniExpr (CUReference, CEVar "m"); 
        CEUniExpr (CUReference, CEVar "sm")]))
    ; CSExpr (CECall ("MyVerifyChecksum", [
        CEUniExpr (CUReference, CEVar "h"); 
        CEUniExpr (CUReference, CEVar "m")]))
    ; CSExpr (CECall ("MyIngress", [
        CEUniExpr (CUReference, CEVar "h"); 
        CEUniExpr (CUReference, CEVar "m"); 
        CEUniExpr (CUReference, CEVar "sm")
      ]))
    ; CSExpr (CECall ("MyEgress", [
        CEUniExpr (CUReference, CEVar "h"); 
        CEUniExpr (CUReference, CEVar "m"); 
        CEUniExpr (CUReference, CEVar "sm")
      ]))
    ; CSExpr (CECall ("MyUpdateChecksum", [
        CEUniExpr (CUReference, CEVar "h"); 
        CEUniExpr (CUReference, CEVar "m")]))
    ; CSExpr (CECall ("MyDeparser", [CEUniExpr (CUReference, CEVar "h")]))
  ])
  ])
let%expect_test _ = F.fprintf F.std_formatter "%a" pp_program sample_program;
  [%expect {|
    #include <cluj_core.h>
    #include <cluj.h>
    #include <endian.h>
    #include <stdbool.h>
    #include <stdint.h>
    #include <stdlib.h>
    #include <unistd.h>

    struct metadata {
      char ok;
    };

    struct headers {

    };

    void MyParser(uint16_t packet_addr,
                  struct headers* hdr,
                  struct metadata* meta,
                  struct standard_metadata_t* standard_metadata) {
      start:
      goto accept;
      accept:
      reject:
      return;
    }

    void MyVerifyChecksum(struct headers* hdr, struct metadata* meta) {
      return;
    }

    void MyIngress(struct headers* hdr,
                   struct metadata* meta,
                   struct standard_metadata_t* standard_metadata) {
      (*standard_metadata).egress_spec = 2;
    }

    void MyEgress(struct headers* hdr,
                  struct metadata* meta,
                  struct standard_metadata_t* standard_metadata) {
      return;
    }

    void MyUpdateChecksum(struct headers* hdr, struct metadata* meta) {
      return;
    }

    void MyDeparser(struct headers* hdr) {
      return;
    }

    int main() {
      uint16_t pkt_address;
      pkt_address = get_packet_addr();
      struct headers h;
      struct metadata m;
      struct standard_metadata_t sm;
      MyParser(pkt_address, (&h), (&m), (&sm));
      MyVerifyChecksum((&h), (&m));
      MyIngress((&h), (&m), (&sm));
      MyEgress((&h), (&m), (&sm));
      MyUpdateChecksum((&h), (&m));
      MyDeparser((&h));
    } |}]


  