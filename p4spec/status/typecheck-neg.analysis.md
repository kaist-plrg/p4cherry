Analysis of p4c negative typechecker tests, lists the "critical" premise that was not satisfied.
If there is no counterpart for the "critical" premise in the spec, it is marked as `(CF)`.

## A. Free Identifier (Not found) or Duplicate Identifier (Already defined)

### (1) Free Identifier

##### call1_e.p4
##### functors2_e.p4
##### functors3_e.p4
##### interface1_e.p4
##### interface_e.p4

```
;; fund_func
(EL) $find_func<...>(...)
(IL) -- if (fid, V, id*)? matches (_)
(CF) -- if (fid, V, id*)? matches ()
```

Structs do not have a constructor, and controls are not reursive.
More of a free-identifer error, difficult to connect to this test property from spec level.
But is this essential at the first place?

##### function_e.p4
##### issue2413.p4

```
;; find_func
(EL) -- if (fid', V', id_deft*) = $find_func_opt<V>(id_f, (id_a?)*, MAP (PAIR fid V)*)
(IL) -- if (fid, V, id*)? matches (_)
```

Fails to find a function because not enough arguments were provided.
Relates to overload resolution rules.

##### incorrect-label.p4

```
;; find_map
(EL) -- if V_key = $find_map_opt<K, V>(MAP (PAIR K V)*, K_key)
(IL) -- if V? matches (_)
(CF) -- if V? matches ()
```

Switch labels on a table application result must be the actions of the table.

##### issue2544_self_ref.p4

```
;; $find_map
(EL) -- if V_key = $find_map_opt<K, V>(MAP (PAIR K V)*, K_key)
(IL) -- if V? matches (_)
(CF) -- if V? matches ()
```

Trying to recursively use an extern type being defined, as a parameter type of its method.

### (2) Duplicate Identifier

##### dup-param.p4
##### dup-param3.p4
##### issue2267.p4

```
;; add_styp
(EL) -- if ~$in_set<id>(id, $dom_map<id, styp>(C.BLOCK.FRAME))
(IL) -- if ~ $in_set<id>(id, $dom_map<id, styp>(C.BLOCK.FRAME))
(CF) -- if $in_set<id>(id, $dom_map<id, styp>(C.BLOCK.FRAME))
```

Conflicting constructor parameter and apply parameter names.
For `issue2267.p4`, apply parameter of a control and the table name are in conflict.

##### dupConst.p4

```
;; add_styp
(EL) -- if ~$in_set<id>(id, $dom_map<id, styp>(C.GLOBAL.FRAME))
(IL) -- if ~ $in_set<id>(id, $dom_map<id, styp>(C.GLOBAL.FRAME))
(CF) -- if $in_set<id>(id, $dom_map<id, styp>(C.GLOBAL.FRAME))
```

Conflicting constant names at the global scope.

##### issue1932-1.p4
##### issue1932-2.p4

```
;; add_funcdef_non_overload
(EL) -- if ~(id <- id_k*)
(IL) -- if ~ id <- id_k*{id_k <- id_k*}
(CF) -- if id <- id_k*{id_k <- id_k*}
```

Actions cannot be overloaded.

## B. Well-formedness Checks

##### default-param.p4

```
;; Paramtype_wf/some
(EL) -- if dir = IN \/ dir = NO 
(IL) -- if ((dir = `IN`_dir()) \/ (dir = `NO`_dir()))
(CF) -- if ((dir =/= `IN`_dir()) /\ (dir =/= `NO`_dir()))
```

Parameters with a default value must be either `IN` or `NO` direction.

##### directionless.p4

```
;; Functype_wf/actiont
(EL) -- if $is_trailing_action(dir*)
(IL) -- if $is_trailing_action(dir*{dir <- dir*})
(CF) -- if ~$is_trailing_action(dir*{dir <- dir*})
```

Directionless arguments must be trailing in an action signature.
May falsify this by reordering the parameters.

##### extern.p4

```
;; Paramtype_wf/none
(EL) -- if $is_externt($canon_typ(typ)) => (dir = NO)
(IL) -- if ($is_externt($canon_typ(typ)) => (dir = `NO`_dir()))
(CF) -- if $is_externt($canon_typ(typ)) /\ (dir =/= `NO`_dir())
```

Paramter of type `extern` cannot have a direction.

##### header2_e.p4

```
;; nestable_structt
(EL) -- if $nestable_structt(typ)*
(IL) -- if $nestable_structt(typ*{typ <- typ*})
(CF) -- if exists ~$nestable_structt(typ*{typ <- typ*})
```

Parser types cannot be nested inside struct types.

##### header3.p4

```
;; nestable_headert
(EL) -- if $nestable_headert(typ)*
(IL) -- if $nestable_headert(typ*{typ <- typ*})
(CF) -- if exists ~$nestable_headert(typ*{typ <- typ*})
```

Tuple types cannot be nested inside header types.

##### issue1296.p4

```
;; Paramtype_wf/some
(EL) -- if $is_externt($canon_typ(typ)) => (dir = NO)
(IL) -- if ($is_externt($canon_typ(typ)) => (dir = `NO`_dir()))
(CF) -- if $is_externt($canon_typ(typ)) /\ (dir =/= `NO`_dir())
```

Similar as `extern.p4`, however this error is second-order.
An extern is passed as a type argument to an extern, where it uses the type parameter as a direction `in` argument of its method.

##### issue2290.p4

```
;; Type_wf/headert
(EL) -- if $nestable_headert(typ)*
(IL) -- (if $nestable_headert(typ))*{typ <- typ*}
(CF) -- if exists ~$nestable_headert(typ*{typ <- typ*})
```

Header should not be nested inside another header type.

##### issue2354-1.p4
##### issue2354.p4

```
FuncType_wf/actiont
(EL) -- if (~$is_deft(typ') /\ ~$is_spect(typ') /\ ~$is_intt(typ')
         /\ ~$is_obj(typ')
         /\ (~$is_strt(typ') \/ (dir = NO)))*
(IL) -- (if ((((~ $is_deft(typ') /\ ~ $is_spect(typ')) /\ ~ $is_intt(typ'))
        /\ ~ $is_obj(typ'))
        /\ (~ $is_strt(typ') \/ (dir = `NO`_dir()))))*{dir <- dir*, typ' <- typ'*}
(CF) -- omitted ...
```

Action cannot take a parameter of type `int`.
For `issue2354-1.p4` the `int` type paramter is directionless and for `issue2354.p4`, it is `in`.

##### issue2454.p4

```
;; Functype_wf/functiont
(EL) -- if (~$is_deft(typ') /\ ~$is_spect(typ') /\ ~$is_obj(typ')
        /\ (~($is_strt(typ') \/ $is_intt(typ'))
        \/ (dir = NO)))*
(IL) -- (if (((~ $is_deft(typ') /\ ~ $is_spect(typ')) /\ ~ $is_obj(typ'))
        /\ (~ ($is_strt(typ') \/ $is_intt(typ'))
        \/ (dir = `NO`_dir()))))*{dir <- dir*, typ' <- typ'*}
(CF) -- omitted ...
```

After type specialization, the function becomes a function which takes a parameter of type `control`.



## C. Call-site Checks

##### call-table.p4

```
;; Call_site_ok/local-tableapplymethod
(EL) -- if C.LOCAL.KIND = CONTROLAPPLYMETHOD
        \/ C.LOCAL.KIND = TABLEAPPLYMETHOD
(IL) -- if ((C.LOCAL.KIND = `CONTROLAPPLYMETHOD`_lkind())
            \/ (C.LOCAL.KIND = `TABLEAPPLYMETHOD`_lkind()))
(CF) -- if ((C.LOCAL.KIND =/= `CONTROLAPPLYMETHOD`_lkind())
            /\ (C.LOCAL.KIND =/= `TABLEAPPLYMETHOD`_lkind()))
```

Cannot invoke a table from an action.

##### control-verify.p4

```
;; Stmt_ok/callfuncs
(EL) -- if (name = CURRENT "verify")
        => ((p = BLOCK /\ C.BLOCK.KIND = PARSER) \/ (p = LOCAL /\ C.LOCAL.KIND = PARSERSTATE))
(IL) -- if ((name = `CURRENT%`_name("verify"))
        => (((p = `BLOCK`_cursor()) /\ (C.BLOCK.KIND = `PARSER`_bkind()))
            \/ ((p = `LOCAL`_cursor()) /\ (C.LOCAL.KIND = `PARSERSTATE`_lkind()))))
(CF) -- if ((name = `CURRENT%`_name("verify"))
            /\ ((p = `GLOBAL`_cursor())
                \/ ((p = `BLOCK`_cursor()) /\ (C.BLOCK.KIND =/= `PARSER`_bkind()))
                \/ ((p = `LOCAL`_cursor()) /\ (C.LOCAL.KIND =/= `PARSERSTATE`_lkind()))))
```

`verify` invocation is only allowed inside a parser block or a parser state.

##### function_e2.p4

```
;; Call_site_ok/local-action
(EL) -- if C.LOCAL.KIND = ACTION
        \/ C.LOCAL.KIND = CONTROLAPPLYMETHOD
        \/ C.LOCAL.KIND = TABLEAPPLYMETHOD
(IL) -- if (((C.LOCAL.KIND = `ACTION`_lkind())
             \/ (C.LOCAL.KIND = `CONTROLAPPLYMETHOD`_lkind()))
            \/ (C.LOCAL.KIND = `TABLEAPPLYMETHOD`_lkind()))
(CF) -- if ((C.LOCAL.KIND =/= `ACTION`_lkind())
            /\ (C.LOCAL.KIND =/= `CONTROLAPPLYMETHOD`_lkind())
            /\ (C.LOCAL.KIND =/= `TABLEAPPLYMETHOD`_lkind()))
```

One cannot invoke an action from a function.

##### issue1331.p4

```
;; Call_site_ok/local-parserapplymethod
(EL) -- if C.LOCAL.KIND = PARSERSTATE
(IL) -- if (C.LOCAL.KIND = `PARSERSTATE`_lkind())
(CF) -- if (C.LOCAL.KIND =/= `PARSERSTATE`_lkind())
```

Cannot invoke a parser from a control.

##### issue2597.p4

```
;; Call_site_ok/local-controlapplymethod
(EL) -- if C.LOCAL.KIND = CONTROLAPPLYMETHOD
(IL) -- if (C.LOCAL.KIND = `CONTROLAPPLYMETHOD`_lkind())
(CF) -- if (C.LOCAL.KIND =/= `CONTROLAPPLYMETHOD`_lkind())
```

A control cannot be invoked from an action.

## D. Table Property Checks

### (1) Action Property

##### action-bind2.p4
##### action-bind3.p4

```
;; Call_action_partial_ok
(EL) -- if |paramIL_d*| = |argIL*|
(IL) -- if (|paramIL_d*{paramIL_d <- paramIL_d*}| = |argIL*{argIL <- argIL*}|)
(CF) -- if (|paramIL_d*{paramIL_d <- paramIL_d*}| < |argIL*{argIL <- argIL*}|)
(CF) -- if (|paramIL_d*{paramIL_d <- paramIL_d*}| > |argIL*{argIL <- argIL*}|)
```

Arity mismatch on partial action invocation at table action property.
Table action property must provide all non-directionless arguments.

### (2) Default Property

##### action-bind.p4

```
;; Call_default_action_ok
(EL) -- if (argIL_a = argIL_d)*
(IL) -- (if (argIL_a = argIL_d))*{argIL_a <- argIL_a*, argIL_d <- argIL_d*}
(CF) -- if exists (argIL_a =/= argIL_d){argIL_a <- argIL_a*, argIL_d <- argIL_d*}*
```

Default action's arguments must agree with the table's declared action arguments.

##### action-bind1.p4

```
;; Call_default_action_ok
(EL) -- if |pt*| = |argIL*|
(IL) -- if (|pt*{pt <- pt*}| = |argIL*{argIL <- argIL*}|)
(CF) -- if (|pt*{pt <- pt*}| < |argIL*{argIL <- argIL*}|)
```

Arity mismatch on partial action invocation at table default property.

##### default_action.p4
##### default_action1.p4
##### issue2033.p4

```
;; Table_default_ok
(EL) -- if (pt*, argIL_a*) = $find_action(tblctx, name)
(IL) -- if (paramtyp*, argIL*)? matches (_)
(CF) -- if (paramtyp*, argIL*)? matches ()
```

Default action must be present in the actions property of a table.
`default_action1.p4` uses the top-level (`.`) namespace specifier.
`issue2033.p4` has a wrong ordering of table properties, where `default_action` comes before the `actions` property.

### (3) Entry Property

##### init-entries-error1-bmv2.p4

```
;; Table_entry_priority_ok/priority-values-nil-somep
(EL) -- if tblctx.MODE = PRI \/ tblctx.MODE = PRILPM
(IL) -- if ((tblctx.MODE = `PRI`_mode()) \/ (tblctx.MODE = `PRILPM`_mode()))
(CF) -- if (tblctx.MODE =/= `PRI`_mode()) /\ (tblctx.MODE =/= `PRILPM`_mode())
```

All-exact match should not have explicit priority values.
Difficult because `tblctx.MODE` is set indirectly.

##### init-entries-error2-bmv2.p4

```
;; Table_entry_priority_ok/else-somep:
(EL) -- if tblctx.PRIORITIES.INIT
(IL) -- if tblctx.PRIORITIES.INIT
(CF) -- if ~tblctx.PRIORITIES.INIT
```

If there is at least one entry with an explicit priority given,
the first entry must have and explicit priority value given.

##### init-entries-error3-bmv2.p4

```
;; Table_entry_priority_ok/else-somep:
(EL) -- if $(i' >= 0)
(IL) -- if (i >= 0 as int)
(CF) -- if (i < 0 as int)
```

Priority values must be positive integers.
In this particular test, because of `largest_priority_wins = true` and `priority_delta = 10`,
the initial priority `12`, the third entry to the last entry gets assigned negative priority values.

##### issue1542.p4

```
;; Call_default_action_ok
(EL) -- if (argIL_a = argIL_d)*
(IL) -- (if (argIL_a = argIL_d))*{argIL_a <- argIL_a*, argIL_d <- argIL_d*}
(CF) -- if exists (argIL_a =/= argIL_d){argIL_a <- argIL_a*, argIL_d <- argIL_d*}*
```

When specifying an action of a table entry, the action must come from the table's action property.
Similar to `action-bind.p4`, but this time the table entry is wrong.

### (4) Custom Property

##### issue1230.p4

```
;; Table_custom_ok/size
(EL) -- if $is_intt(typ) \/ $is_fintt(typ) \/ $is_fbitt(typ)
(IL) -- if (($is_intt(typ) \/ $is_fintt(typ)) \/ $is_fbitt(typ))
(CF) -- if ~$is_intt(typ) /\ ~$is_fintt(typ) /\ ~$is_fbitt(typ)
```

A table's `size` proprety must be either `int`, `int<w>`, or `bit<w>`.

## E. Alpha-equivalence, Subtype, and L-value Checks

### (1) Alpha-equivalence

##### constructor3_e.p4

Trying to pass a `bool` type argument where `bit` is expected as a directionless argument.
Because directionless arguments are not implicitly casted, alpha-equivalence check applies.

##### issue2230-1-bmv2.p4
##### issue2230.p4

Trying to equate structurally equivalent struct/header with different names.
Because in P4 `struct`/`header` are nominal, they should not be equal.

### (2) Subtype Checks

##### const.p4

`bit<1>` to `bit<32>`.

##### issue2332.p4

`bit<1>` to `bit<32>`.

##### const1_e.p4

`bit<16>` to `bit<32>`.

##### equality-fail.p4

`varbit<32>` to `header`.

##### implicit.p4

`int<32>` to `bit<32>`.

##### issue1006-1.p4

`bit<16>` to `bit<8>`.

##### issue1986-1.p4

`bit<0>` to `bit<1>`.

##### issue1986.p4

`bit<0>` to `bit<2>`.

##### issue2036-1.p4
##### issue2036-2.p4

`tuple` to `struct`.

##### issue2036.p4

`struct` to `tuple`.

##### issue2220.p4

`bit<8>` to a serializable enum with underlying type `bit<8>`.

##### issue2444-1.p4

`bit<4>` to `int`.

### (3) L-value Checks

##### function1_e.p4

Trying to pass `1w1 & 1w0` as a l-value.

##### inro.p4

```
;; Lval_ok/namee
(EL) -- if dir = OUT \/ dir = INOUT
(IL) -- if ((dir = `OUT`_dir()) \/ (dir = `INOUT`_dir()))
(CF) -- if ((dir =/= `OUT`_dir()) /\ (dir =/= `INOUT`_dir()))
```

Left-hand side of an assignment, if a variable, must be of `OUT` or `INOUT` direction.

## F. Miscellaneous Checks

### (1) Expression

##### binary_e.p4

```
;; Expr_ok/arracce-tuplet, Expr_ok/arracce-stackt-lctk, Expr_ok/arracce-stackt-ctk-dyn
(EL) -- if TupleT typ_b'* = $canon_typ(typ_b)
(EL) -- if StackT typ_b' i_s = $canon_typ(typ_b)
(IL) -- if typ' <: datatyp
(CF) -- if typ' </: datatyp
```

Array access expression is defined on tuple and stack types only.

##### bitExtract_e.p4

```
;; Expr_ok/bitacce
(EL) -- if exprIL_b' = $reduce_senum_unary(exprIL_b, def $compatible_bitstringbase)
```

Involves SpecTec function arguments.

##### div.p4

```
;; Expr_ok/bine-div-mod-lctk
(EL) -- if n_r = $get_num(val_r)
(IL) -- if int <: nat
(CF) -- if int < 0
```

If the right-hand side of division is local compile-time known, it must be a positive integer.

##### div0.p4
##### div1.p4
##### div3.p4

```
;; Expr_ok/bine-div-mod-lctk
(EL) -- if (exprIL_l'', exprIL_r'') = $reduce_senums_binary(exprIL_l', exprIL_r', def $compatible_divmod)
```

Division is not defined on fixed width bit type.
Involves SpecTec function arguments.

##### issue1059.p4
##### issue1557-bmv2.p4

```
;; Expr_ok/expracce-structt
(EL) -- if typ = $assoc_<member, typ>(member, (member_s, typ_s)*)
(IL) -- if typ? matches (_)
(CF) -- if typ? matches ()
```

Trying to access a non-existent member of a struct.

##### issue2317.p4

```
;; Expr_ok/callfunce
(EL) -- if typ =/= VoidT
(IL) -- if (typ =/= `VoidT`_primtyp() as typ)
(CF) -- if (typ = `VoidT`_primtyp() as typ)
```

Function call as an expression must not return `void`.

### (2) Statement

##### assign.p4
##### issue2021.p4

```
;; Stmt_ok/assigns
(EL) -- if `(typ_l; DYN) = $annot(exprIL_l)
(IL) -- if (ctk = `DYN`_ctk())
(CF) -- if (ctk =/= `DYN`_ctk())
     -- i.e., ((ctk = `CTK`_ctk()) \/ (ctk = `LCTK`_ctk()))
```

The left-hand side of an assignment must be dynamic.

##### conditional_e.p4

```
;; Stmt_ok/ifs
(EL) -- if `(BoolT; _) = $annot(exprIL_c)
(IL) -- if (typ = `BoolT`_primtyp() as typ)
(CF) -- if (typ =/= `BoolT`_primtyp() as typ)
```

Condition expression of an if statement must be of boolean type.

##### default-last-switch.p4
##### issue2525.p4

```
;; Switch_tbl_label_ok/defaultl
(EL) p C id_t true |- DefaultL : DefaultL
(IL) -- if (bool = true)
(CF) -- if (bool = false)
```

Default label must be the last label in a switch statement.
But negating this seems difficult, as `bool` is indireclty defined.
For `issue2525.p4`, there are two trailing `default` labels.

##### duplicate-label.p4

```
;; Stmt_ok/switchs-gen
(EL) -- if $distinct_<switchlabel>(switchlabel*)
(IL) -- if $distinct_<switchlabel>(switchlabel*{switchlabel <- switchlabel*})
(CF) -- if ~$distinct_<switchlabel>(switchlabel*{switchlabel <- switchlabel*})
```

Switch labels must be distinct.

### (3) Declaration

##### accept_e.p4

```
;; Decl_ok/parserd
(EL) -- if ~("accept" <- statelabel*)
(IL) -- if ~ "accept" <- statelabel*{statelabel <- statelabel*}
(CF) -- if "accept" <- statelabel*
```

An "accept" state is reserved.

##### const_e.p4

```
;; Decl_ok/constd
(EL) -- if `(_; LCTK) = $annot(exprIL)
(IL) -- if (ctk = `LCTK`_ctk())
(CF) -- if (ctk =/= `LCTK`_ctk())
     -- i.e., ((ctk = `CTK`_ctk()) \/ (ctk = `DYN`_ctk()))
```

Trying to define a non local-compile time konwn value as constant.

##### constructor_e.p4

```
;; Method_ok/externm
(EL) C_0 id_e |- ExternM ...
(IL) -- if (id_e =/= id)
(CF) -- if (id_e = id)
```

Extern constructors should not have return types.
Extern methods should not have the same name as the extern object being defined.

##### function_e1.p4

```
;; Decl_ok/funcd
(EL) -- if f = RET \/ (f = CONT /\ typ_r = VoidT)
(IL) -- if ((f = `RET`_flow()) \/ ((f = `CONT`_flow()) /\ (typ_r = `VoidT`_primtyp() as typ))) 
(CF) -- if ((f =/= `RET`_flow()) /\ ((f =/= `CONT`_flow()) \/ (typ_r =/= `VoidT`_primtyp() as typ)))
     -- i.e., (f = `CONT`_flow()) /\ (typ_r =/= `VoidT`_primtyp() as typ)
```

A function must return a value on every possible execution paths, if its return type is not `void`.

##### issue2335.p4

```
;; Decl_ok/vard-some
(EL) -- if $is_assignable(typ)
(IL) -- if $is_assignable(typ)
(CF) -- if ~$is_assignable(typ)
```

Trying to declare a variable of type `int`, which is illegal.
