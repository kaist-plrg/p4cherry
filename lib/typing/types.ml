open Syntax.Ast
open Util.Source

module Type = struct
  type t =
    (* Base types *)
    | VoidT
    | ErrT
    | MatchKindT
    | StrT
    | BoolT
    | AIntT
    | IntT of Bigint.t
    | BitT of Bigint.t
    | VBitT of Bigint.t
    | TupleT of t list
    | StackT of t * Bigint.t
    (* Parametrized types *)
    | VarT of id'
    (* Alias types *)
    | DefT of t
    | NewT of t
    (* Aggregate types *)
    | StructT of (member' * t) list
    | HeaderT of (member' * t) list
    | UnionT of (member' * t) list
    (* (TODO) maybe just id suffices *)
    | EnumT of member' list
    | SEnumT of t * (member' * Runtime.Value.t) list
    (* Object types *)
    | ExternT
    | ParserT of param' list
    | ControlT of param' list
    | PackageT
    (* Top type *)
    | TopT

  let rec pp fmt = function
    (* Base types *)
    | VoidT -> Format.fprintf fmt "void"
    | ErrT -> Format.fprintf fmt "error"
    | MatchKindT -> Format.fprintf fmt "match_kind"
    | StrT -> Format.fprintf fmt "string"
    | BoolT -> Format.fprintf fmt "bool"
    | AIntT -> Format.fprintf fmt "int"
    | IntT n -> Format.fprintf fmt "int<%a>" Bigint.pp n
    | BitT n -> Format.fprintf fmt "bit<%a>" Bigint.pp n
    | VBitT n -> Format.fprintf fmt "vbit<%a>" Bigint.pp n
    | TupleT ts ->
        Format.fprintf fmt "tuple<%a>"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          ts
    | StackT (t, n) -> Format.fprintf fmt "stack %a[%a]" pp t Bigint.pp n
    (* Parametrized types *)
    | VarT id -> Format.fprintf fmt "%s" id
    (* Alias types *)
    | DefT t -> Format.fprintf fmt "typedef %a" pp t
    | NewT t -> Format.fprintf fmt "type %a" pp t
    (* Aggregate types *)
    | StructT fields ->
        Format.fprintf fmt "struct { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info) pp
                 t))
          fields
    | HeaderT fields ->
        Format.fprintf fmt "header { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info) pp
                 t))
          fields
    | UnionT fields ->
        Format.fprintf fmt "union { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info) pp
                 t))
          fields
    | EnumT members ->
        Format.fprintf fmt "enum { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt m ->
               Format.fprintf fmt "%a" Syntax.Pp.pp_member (m $ no_info)))
          members
    | SEnumT (t, members) ->
        Format.fprintf fmt "enum %a { %a }" pp t
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, v) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 Runtime.Value.pp v))
          members
    (* Object types *)
    | ExternT -> Format.fprintf fmt "extern"
    | ParserT _ -> Format.fprintf fmt "parser"
    | ControlT _ -> Format.fprintf fmt "control"
    | PackageT -> Format.fprintf fmt "package"
    (* Top type *)
    | TopT -> Format.fprintf fmt "top"
end

module FuncType = struct
  type t = Type.t list * Type.t

  let pp fmt _t = Format.fprintf fmt "functype"
end

module ConsType = struct
  type t = Type.t list * Type.t

  let pp fmt _t = Format.fprintf fmt "constype"
end

module TypeDef = struct
  type t =
    (* Aliased type definitions *)
    | DefD of Type.t
    | NewD of Type.t
    (* Aggregate type definitions *)
    (* These will become generic in the future *)
    | StructD of (member' * Type.t) list
    | HeaderD of (member' * Type.t) list
    | UnionD of (member' * Type.t) list
    | EnumD of member' list
    | SEnumD of Type.t * (member' * Runtime.Value.t) list
    (* Object type definitions *)
    | ExternD of tparam' list
    | ParserD of tparam' list * param' list
    | ControlD of tparam' list * param' list
    | PackageD of tparam' list

  let pp fmt = function
    | DefD t -> Format.fprintf fmt "typedef %a" Type.pp t
    | NewD t -> Format.fprintf fmt "type %a" Type.pp t
    | StructD fields ->
        Format.fprintf fmt "struct { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 Type.pp t))
          fields
    | HeaderD fields ->
        Format.fprintf fmt "header { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 Type.pp t))
          fields
    | UnionD fields ->
        Format.fprintf fmt "union { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 Type.pp t))
          fields
    | EnumD members ->
        Format.fprintf fmt "enum { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt m ->
               Format.fprintf fmt "%a" Syntax.Pp.pp_member (m $ no_info)))
          members
    | SEnumD (t, members) ->
        Format.fprintf fmt "enum %a { %a }" Type.pp t
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, v) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 Runtime.Value.pp v))
          members
    | ExternD _ -> Format.fprintf fmt "extern"
    | ParserD _ -> Format.fprintf fmt "parser"
    | ControlD _ -> Format.fprintf fmt "control"
    | PackageD _ -> Format.fprintf fmt "package"
end
