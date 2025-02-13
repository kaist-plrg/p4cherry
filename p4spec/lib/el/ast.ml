open Xl
open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Num.t

(* Texts *)

type text = string

(* Identifiers *)

type id = id' phrase
and id' = string

(* Atoms *)

type atom = Atom.t phrase

(* Iterators *)

type iter =
  | Opt       (* `?` *)
  | List      (* `*` *)

(* Types *)

and plaintyp = plaintyp' phrase
and plaintyp' =
  | VarT of id * targ list      (* id (`<` list(targ, `,`) `>`)? *)
  | BoolT                       (* `bool` *)
  | NumT of Num.typ             (* numtyp *)
  | TextT                       (* `text` *)
  | ParenT of plaintyp          (* `(` plaintyp `)` *)
  | TupT of plaintyp list       (* `(` list(plaintyp, `,`) `)` *)
  | IterT of plaintyp * iter    (* plaintyp iter *)

(* What are the consequences of disallowing iterated notation types? *)

and nottyp = nottyp' phrase
and nottyp' =
  | PlainT of plaintyp                (* plaintyp *)
  | AtomT of atom                     (* atom *)
  | SeqT of nottyp list               (* list(nottyp, ` `) *)
  | InfixT of nottyp * atom * nottyp  (* nottyp atom nottyp *)
  | BrackT of atom * nottyp * atom    (* ``[({` nottyp `})]` *)

and deftyp = deftyp' phrase
and deftyp' =
  | NotationT of nottyp        (* nottyp *)
  | StructT of typfield list   (* `{` list(typfield, `,`) `}` *)
  | VariantT of typcase list   (* `|` list(typcase, `|`) *)

and typfield = atom * plaintyp list * hint list
and typcase = nottyp * hint list

(* Operators *)

and numop = [ `DecOp | `HexOp ]
and unop = [ Bool.unop | Num.unop ]
and binop = [ Bool.binop | Num.binop ]
and cmpop = [ Bool.cmpop | Num.cmpop ]

(* Expressions *)

and exp = exp' phrase
and exp' =
  | BoolE of bool                       (* bool *)
  | NumE of numop * num                 (* num *)
  | TextE of text                       (* text *)
  | VarE of id * targ list              (* id (`<` list(targ, `,`) `>`)? *)
  | UnE of unop * exp                   (* unop exp *)
  | BinE of exp * binop * exp           (* exp binop exp *)
  | CmpE of exp * cmpop * exp           (* exp cmpop exp *)
  | EpsE                                (* `eps` *)
  | ListE of exp list                   (* `[` exp* `]` *)
  | IdxE of exp * exp                   (* exp `[` exp `]` *)
  | SliceE of exp * exp * exp           (* exp `[` exp `:` exp `]` *)
  | UpdE of exp * path * exp            (* exp `[` path `=` exp `]` *)
  | StrE of (atom * exp) list           (* `{` list(atom exp, `,`) `}` *)
  | DotE of exp * atom                  (* exp `.` atom *)
  | CommaE of exp * exp                 (* exp `,` exp *)
  | CatE of exp * exp                   (* exp `++` exp *)
  | MemE of exp * exp                   (* exp `<-` exp *)
  | LenE of exp                         (* `|` exp `|` *)
  | ParenE of exp                       (* `(` exp `)` *)
  | TupE of exp list                    (* `(` list2(exp, `,`) `)` *)
  | CallE of id * targ list * arg list  (* `$` defid (`<` list(targ, `,`) `>`)? (`(` list(arg, `,`) `)`)? *)
  | IterE of exp * iter                 (* exp iter *)
  | TypE of exp * plaintyp              (* exp `:` typ *)
  | ArithE of exp                       (* `$(` exp `)` *)
  (* Notation expressions *)
  | AtomE of atom                       (* atom *)
  | SeqE of exp list                    (* list(exp, ` `) *)
  | InfixE of exp * atom * exp          (* exp atom exp *)
  | BrackE of atom * exp * atom         (* ``[({` exp `})]` *)
  (* Hint expressions *)
  | HoleE of [ `Num of int | `Next | `Rest | `None ]  (* `%N` or `%` or `%%` or `!%` *)
  | FuseE of exp * exp                                (* exp `#` exp *)
  | UnparenE of exp                                   (* `##` exp *)
  | LatexE of string                                  (* `latex` `(` `"..."`* `)` *)

(* Paths *)

and path = path' phrase
and path' =
  | RootP                        (*  *)
  | IdxP of path * exp           (* path `[` exp `]` *)
  | SliceP of path * exp * exp   (* path `[` exp `:` exp `]` *)
  | DotP of path * atom          (* path `.` atom *)

(* Parameters *)

and param = param' phrase
and param' =
  (* plaintyp *)
  | ExpP of plaintyp
  (* `def` `$`id ` (`<` list(tparam, `,`) `>`)? (`(` list(param, `,`) `)`)? `:` plaintyp *)
  | DefP of id * tparam list * param list * plaintyp

(* Type parameters *)

and tparam = tparam' phrase
and tparam' = id'

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp   (* exp *)
  | DefA of id    (* `$`id *)

(* Type arguments *)

and targ = targ' phrase
and targ' = plaintyp'

(* Premises *)

and prem = prem' phrase
and prem' =
  | VarPr of id * plaintyp         (* id `:` plaintyp *)
  | RulePr of id * exp             (* id `:` exp *)
  | IfPr of exp                    (* `if` exp *)
  | ElsePr                         (* `otherwise` *)
  | LetPr of exp * exp             (* `let` exp `=` exp *)
  | IfLetPr of exp * exp           (* `iflet` exp `=` exp *)
  | IterPr of prem * iter          (* prem iter *)

(* Hints *)

and hint = { hintid : id; hintexp : exp }

(* Definitions *)

type def = def' phrase
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` *)
  | SynD of id * tparam list
  (* `syntax` id `<` list(tparam, `,`) `>` hint* `=` deftyp *)
  | TypD of id * tparam list * deftyp * hint list
  (* `var` id `:` plaintyp hint* *)
  | VarD of id * plaintyp * hint list
  (* `relation` id `:` nottyp hint* *)
  | RelD of id * nottyp * hint list
  (* `rule` id`/`id `:` exp list(`--` prem, nl) *)
  | RuleD of id * id * exp * prem list
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` plaintyp hint* *)
  | DecD of id * tparam list * param list * plaintyp * hint list
  (* `def` id `<` list(targ, `,`) `>` list(arg, `,`) `=` exp list(`--` prem, nl) *)
  | DefD of id * targ list * arg list * exp * prem list
  | SepD

(* Spec *)

type spec = def list
