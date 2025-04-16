Analysis of p4c negative typechecker tests, lists the "critical" premise that was not satisfied.
If there is no counterpart for the "critical" premise in the spec, it is marked as `(CF)`.

## A. Free Identifier (Not found) or Duplicate Identifier (Already defined)

### (1) Free Identifier

##### call1_e.p4
##### functors2_e.p4
##### functors3_e.p4
##### interface1_e.p4
##### interface_e.p4
##### issue3197-e.p4
```
;; find_func
(EL) $find_func<...>(...)
(IL) -- if (fid, V, id*)? matches (_)
(CF) -- if (fid, V, id*)? matches ()
```

Structs do not have a constructor, and controls are not recursive.
More of a free-identifer error, difficult to connect to this test property from spec level.
But is this essential at the first place?

##### function_e.p4
##### issue2413.p4
##### issue3813.p4
##### issue3814.p4
##### issue803-1.p4
##### named-fail.p4
##### named-fail1.p4
##### not_bound.p4
##### self-call.p4

```
;; find_func
(EL) -- if (fid', V', id_deft*) = $find_func_opt<V>(id_f, (id_a?)*, MAP (PAIR fid V)*)
(IL) -- if (fid, V, id*)? matches (_)
```

Fails to find a function because not enough arguments were provided.
Relates to overload resolution rules.
- issue3813: fails to find a function because optional parameters are not allowed in non-extern functions
- issue3814: fails to find a constructor because optional parameters are not allowed in control declarations
- issue803-1: fails to find a constructor because not enough argnuments were provided. 
- named-fail: fails to find a function because not all arguments are named.
- named_fail1: fails to find a function because parameter names are not found.
- not_bound: fails to find a function because a parameter is not bound.
- self-call: controls are not recursive.

##### incorrect-label.p4

```
;; find_map
(EL) -- if V_key = $find_map_opt<K, V>(MAP (PAIR K V)*, K_key)
(IL) -- if V? matches (_)
(CF) -- if V? matches ()
```

Switch labels on a table application result must be the actions of the table.

##### issue2544_self_ref.p4
##### issue3264.p4
##### missing_match.p4
##### scope.p4
##### spec-issue563.p4

```
;; $find_map
(EL) -- if V_key = $find_map_opt<K, V>(MAP (PAIR K V)*, K_key)
(IL) -- if V? matches (_)
(CF) -- if V? matches ()
```

- issue2544_self_ref: Trying to recursively use an extern type being defined, as a parameter type of its method.
- issue3264: free identifier `this`
- missing_match: free identifier `noSuchMatch`
- scope: free identifier `.q`
- spec-issue563: parameter cannot have control type

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
;; ParamType_wf/some
(EL) -- if dir = IN \/ dir = NO 
(IL) -- if ((dir = `IN`_dir()) \/ (dir = `NO`_dir()))
(CF) -- if ((dir =/= `IN`_dir()) /\ (dir =/= `NO`_dir()))
```

Parameters with a default value must be either `IN` or `NO` direction.

##### package.p4
```
;; CParamType_wf:
(EL) tidset consctxt |- id NO typ val?
(IL) -- if (dir = `NO`_dir())
(CF) -- if (dir =/= `NO`_dir())
```
Constructor parameters must be `NO` direction.

##### directionless.p4

```
;; FuncType_wf/actiont
(EL) -- if $is_trailing_action(dir*)
(IL) -- if $is_trailing_action(dir*{dir <- dir*})
(CF) -- if ~$is_trailing_action(dir*{dir <- dir*})
```

Directionless arguments must be trailing in an action signature.
May falsify this by reordering the parameters.

##### extern.p4
##### issue3787.p4

```
;; ParamType_wf/none
(EL) -- if $is_externt($canon_typ(typ)) => (dir = NO)
(IL) -- if ($is_externt($canon_typ(typ)) => (dir = `NO`_dir()))
(CF) -- if $is_externt($canon_typ(typ)) /\ (dir =/= `NO`_dir())
```

Parameter of type `extern` cannot have a direction.

##### header2_e.p4
##### issue3304.p4

```
;; nestable_structt
(EL) -- if $nestable_structt(typ)*
(IL) -- if $nestable_structt(typ*{typ <- typ*})
(CF) -- if exists ~$nestable_structt(typ*{typ <- typ*})
```
Parser types cannot be nested inside struct types.

##### psa-type-hdr.p4
```
;; Type_wf/newt
(EL) -- if $nestable_newt(typ)
(IL) -- if $nestable_newt(typ)
(CF) -- if ~$nestable_newt(typ)
```
Header types cannot be nested inside new types.

##### header3.p4
##### issue3170-1.p4
##### issue401.p4

```
;; nestable_headert
(EL) -- if $nestable_headert(typ)*
(IL) -- if $nestable_headert(typ*{typ <- typ*})
(CF) -- if exists ~$nestable_headert(typ*{typ <- typ*})
```

- header3: Tuple types cannot be nested inside header types.
- issue3170-1: Bit types cannot be nested inside header types.
- issue401: Error types cannot be nested inside header types.

##### issue3378.p4
##### issue3750-1.p4
##### issue3750.p4
```
;; nestable_tuplet
(EL) -- if $nestable_tuplet(typ)*
(IL) -- if $nestable_tuplet(typ)
(CF) -- if ~$nestable_tuplet(typ)
```

- issue3378: Package types cannot be nested inside tuple types.
- issue3750-1: Extern types cannot be nested inside tuple types.
- issue3750: Control types cannot be nested inside tuple types.

##### issue3808.p4
##### issue584.p4
##### issue764.p4
##### issue807.p4
##### parser-arg.p4
```
;; FuncType_wf/externfunctiont, externmethodt, parserapplymethodt:
(EL) -- if (~$is_deft(typ') /\ ~$is_spect(typ')
(IL) -- if (dir = `NO`_dir())
         /\ ~($is_obj(typ') /\ ~$is_externt(typ'))
         /\ (~($is_strt(typ') \/ $is_intt(typ'))
             \/ (dir = NO)))*
(IL) -- if (((~ $is_deft(typ') /\ ~ $is_spect(typ')) /\ ~ ($is_obj(typ') /\ ~ $is_externt(typ'))) /\ (~ ($is_strt(typ') \/ $is_intt(typ')) \/ (dir = `NO`_dir())))
```

- issue3808: Table types cannot be nested inside extern functions.
- issue584, issue764: Int types cannot be nested inside extern functions.
- issue807: Control types cannot be nested inside extern methods.
- parser-arg: Parser types cannot be nested inside parser apply methods.

##### issue816-1.p4
```
;; ConsType_wf/controlt:
(EL) -- if (~$is_deft(typ_p') /\ ~$is_spect(typ_p')
        /\ ~$is_parsert(typ_p') /\ ~$is_packaget(typ_p')
        /\ ~$is_tablet(typ_p'))*
(IL) -- if (~$is_deft(typ_p') /\ ~$is_spect(typ_p')
        /\ ~$is_parsert(typ_p') /\ ~$is_packaget(typ_p')
        /\ ~$is_tablet(typ_p'))*

```

Package types cannot be nested inside control types.

##### issue818.p4
```
;; ConsType_wf/externt:
(EL) -- if (~$is_deft(typ') /\ ~$is_spect(typ')
        /\ ~($is_obj(typ') /\ ~$is_externt(typ')))*
(IL) -- if (~$is_deft(typ') /\ ~$is_spect(typ')
        /\ ~($is_obj(typ') /\ ~$is_externt(typ')))*
```

Control types cannot be nested inside extern types.

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
##### param.p4

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

- issue2354-1: actions cannot take `int` type paramter (directionless)
- issue2354: (dir: `in`)
- param: Extern types cannot be nested inside actions.

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
##### issue413.p4

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

- function_e2: Cannot invoke an action from a function.
- issue413: Cannot invoke an action from a parser.

##### issue1331.p4

```
;; Call_site_ok/local-parserapplymethod
(EL) -- if C.LOCAL.KIND = PARSERSTATE
(IL) -- if (C.LOCAL.KIND = `PARSERSTATE`_lkind())
(CF) -- if (C.LOCAL.KIND =/= `PARSERSTATE`_lkind())
```

Cannot invoke a parser from a control.

##### issue2597.p4
##### issue816.p4
```
;; Call_site_ok/local-controlapplymethod
(EL) -- if C.LOCAL.KIND = CONTROLAPPLYMETHOD
(IL) -- if (C.LOCAL.KIND = `CONTROLAPPLYMETHOD`_lkind())
(CF) -- if (C.LOCAL.KIND =/= `CONTROLAPPLYMETHOD`_lkind())
```
Cannot invoke `control_apply()` from an action.

##### issue3045.p4
##### issue3045-1.p4
##### issue3045-2.p4
```
;; Call_site_ok/local-externfunc-externabstractmethod, Call_site_ok/local-externmethod-externabstractmethod
(EL) -- if C.LOCAL.KIND = EXTERNABSTRACTMETHOD typ
;; Call_site_ok/local-externfunc-else, Call_site_ok/local-externmethod-else
(EL) -- if C.LOCAL.KIND = ACTION
        \/ C.LOCAL.KIND = PARSERSTATE
        \/ C.LOCAL.KIND = CONTROLAPPLYMETHOD
        \/ C.LOCAL.KIND = TABLEAPPLYMETHOD
(IL) -- if ((((C.LOCAL.KIND = `ACTION`_lkind())
            \/ (C.LOCAL.KIND = `PARSERSTATE`_lkind()))
            \/ (C.LOCAL.KIND = `CONTROLAPPLYMETHOD`_lkind()))
            \/ (C.LOCAL.KIND = `TABLEAPPLYMETHOD`_lkind()))
(CF) -- if (((((C.LOCAL.KIND =/= `ACTION`_lkind())
            /\ (C.LOCAL.KIND =/= `PARSERSTATE`_lkind()))
            /\ (C.LOCAL.KIND =/= `CONTROLAPPLYMETHOD`_lkind()))
            /\ (C.LOCAL.KIND =/= `TABLEAPPLYMETHOD`_lkind()))
```
An extern cannot be invoked from a function.


##### issue3188.p4
```
;; Call_site_ok/block-externfunc
(EL) BLOCK C |- ExternFuncT _ _ : CALLSITE_OK
(IL) -- if cursor = `BLOCK`_cursor()
;; Call_site_ok/local-externfunc-else, Call_site_ok/local-externmethod-else
(EL) LOCAL C |- ExternFuncT _ _ : CALLSITE_OK
(IL) -- if cursor = `LOCAL`_cursor()
(CF) -- if (cursor =/= `LOCAL`_cursor() /\ cursor =/= `BLOCK`_cursor())
```

A (extern) function cannot be invoked from top-level.
`GLOBAL` case for `cursor`is not covered in pattern-matching.

##### issue388.p4
```
;; Inst_site_ok/global:
(EL) -- if $is_externt(typ_i') \/ $is_packaget(typ_i')
(IL) -- if ($is_externt(typ_i') \/ $is_packaget(typ_i'))
(CF) -- if (~$is_externt(typ_i') /\ ~$is_packaget(typ_i'))
```

Instantiations cannot be at top-level.

##### issue3522.p4
##### next.p4
```
;; Expr_ok/expracce-stackt-lastindex, expracce-stackt-last:
(EL) -- if (p = BLOCK /\ C.BLOCK.KIND = PARSER) \/ (p = LOCAL /\ C.LOCAL.KIND = PARSERSTATE)
(IL) -- if (((p = `BLOCK`_cursor()) /\ (C.BLOCK.KIND = `PARSER`_bkind())) \/ ((p = `LOCAL`_cursor()) /\ (C.LOCAL.KIND = `PARSERSTATE`_lkind())))
(CF) -- if (((p =/= `BLOCK`_cursor()) \/ (C.BLOCK.KIND =/= `PARSER`_bkind())) /\ ((p =/= `LOCAL`_cursor()) \/ (C.LOCAL.KIND =/= `PARSERSTATE`_lkind())))

```

- issue3522: `lastIndex` for stacks can only be used within a parser.
- next: `last` for stacks can only be used within a parser.

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

##### issue3727.p4
```
;; Table_action_ok:
(EL) -- if MonoFD (ActionT pt*) = $find_funcdef_by_name(p, C, name)
(IL) cannot `PolyFD%->%` to monofuncdef ()
```
Cannot use functions as an action.

### (2) Default Property

##### action-bind.p4
##### issue3671-2.p4

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

##### issue3442.p4
```
;; Table_entries_ok:
(EL) -- if (|tblctx_0.KEYS| = 0) => (|tblentry*| = 0)
(IL) -- if ((|tblctx_0.KEYS| = 0) => (|tblentry*{tblentry <- tblentry*}| = 0))
(CF) -- if (|tblctx_0.KEYS| = 0 /\ |tblentry*{tblentry <- tblentry*}| =/= 0)
```
Entries cannot be specified for a table with no key.

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

##### module_e.p4
```
;; Type_alpha/parsert:
(EL) -- if (dir_a = dir_b)*
(IL) -- if (dir_a = dir_b)
(CF) -- if (dir_a =/= dir_b)
```
Trying to equate two parser types with different argument direction.

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
##### list-error.p4

`bit<16>` to `bit<32>`.
- list-error: `list<bit<16>>` eq_alpha `list<bit<32>>`

##### equality-fail.p4

`varbit<32>` to `header`.

##### implicit.p4

`int<32>` to `bit<32>`.

##### signed.p4

`int<8>` to `bit<8>`.

##### issue1006-1.p4

`bit<16>` to `bit<8>`.

##### issue1986-1.p4

`bit<0>` to `bit<1>`.

##### issue1986.p4

`bit<0>` to `bit<2>`.

##### signs.p4

`bit<8>` to `int<8>`.

##### issue2036-1.p4
##### issue2036-2.p4

`tuple` to `struct`.

##### issue561-1.p4

`tuple` to `header_union`

##### issue401.p4

`tuple` to `bit<1>`.

##### issue2036.p4

`struct` to `tuple`.

##### issue2220.p4

`bit<8>` to a serializable enum with underlying type `bit<8>`.

##### issue3623-2.p4
`bit<4>` to a serializable enum with underlying type `bit<4>`.

##### issue3534.p4
##### serEnumImplCast.p4
`bit<2>` to a serializable enum with underlying type `bit<2>`.

##### issue3623-3.p4
`int` to a serializable enum with underlying type `bit<4>`.

##### issue2444-1.p4

`bit<4>` to `int`.

##### range_e.p4
`bit<3>` to `bit<2>`.

##### issue3221.p4
`bit<2>` to `bit<1>`.

##### issue3345.p4
`bit<1>` to `bool`.

##### issue67.p4
`int` to `bool`.

##### newtype-err.p4
`typedef bit<32>` to newtype `type bit<32>`.

##### serenum-type.p4
##### serenum-typedef.p4
`int` to newtype `type bit<16>`

##### psa-meter2.p4
`typedef int` to `bit<16>`.

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

##### issue3153.p4
```
;; Expr_ok/une-uplus
(EL) -- if exprIL_e = $reduce_senum_unary(exprIL, def $compatible_uplus)
```

Involves SpecTec function arguments.

##### mux_e.p4
```
;; Expr_ok/terne:
(EL) -- if `(BoolT; ctk_c) = $annot(exprIL_c)
(IL) -- if (typ = ((typ) `BoolT`_primtyp()))
(CF) -- if (typ =/= ((typ) `BoolT`_primtyp()))
```
Condition of ternary expression must be `bool`.

##### bitExtract_e.p4

```
;; Expr_ok/bitacce
(EL) -- if exprIL_b' = $reduce_senum_unary(exprIL_b, def $compatible_bitstringbase)
```

Involves SpecTec function arguments.

##### issue4136.p4
```
;; Expr_ok/bitacce
(EL) -- if n_l = $get_num(val_l)
(IL) downcast val_l to natural number
```
Cannot use negative integers in slice range for integers.

##### slice_out_of_bound.p4
```
;; Expr_ok/bitacce
(EL) -- if $is_valid_bitstring_slice(typ_b', n_l, n_h)
(IL) -- if $is_valid_bitstring_slice(typ_b', n_l, n_h)
(CF) -- if ~$is_valid_bitstring_slice(typ_b', n_l, n_h)
```
Slice range out of bounds

##### binary_e.p4

```
;; Expr_ok/arracce-tuplet, Expr_ok/arracce-stackt-lctk, Expr_ok/arracce-stackt-ctk-dyn
(EL) -- if TupleT typ_b'* = $canon_typ(typ_b)
(EL) -- if StackT typ_b' i_s = $canon_typ(typ_b)
(IL) -- if typ' <: datatyp
(CF) -- if typ' </: datatyp
```

Array access expression is defined on tuple and stack types only.

##### stack-const-index-out-of-bounds-bmv2.p4
```
;; Expr_ok/arracce-stackt-lckt
(EL) -- if $(0 <= i) /\ $(i <= i_s)
(IL) -- if ((((int) 0) <= i) /\ (i <= i_s))
(CF) -- if ((((int) 0) > i) \/ (i > i_s))
```
Index out of bounds for stacks.

##### div.p4

```
;; Expr_ok/bine-div-mod-lctk
(EL) -- if n_r = $get_num(val_r)
(IL) -- if int <: nat
(CF) -- if int < 0
```

If the right-hand side of division is local compile-time known, it must be a positive integer.

##### issue3316.p4
```

;; Param_ok/param-default:
(EL) -- if `(typ_e; LCTK) = $annot(exprIL_e)
(IL) -- if ctk = `LCTK`_ctk()
(CF) -- if ctk =/= `LCTK`_ctk()
```

Default parameters for functions should be local compile-time known.

##### div0.p4
##### div1.p4
##### div3.p4
##### neg.p4
```
;; Expr_ok/bine-div-mod-lctk
(EL) -- if (exprIL_l'', exprIL_r'') = $reduce_senums_binary(exprIL_l', exprIL_r', def $compatible_divmod)
```

- div0, div1, div3: Division is not defined on fixed width bit type.
- neg: Division is not defined on signed int type.
Involves SpecTec function arguments.

##### spec-issue1297-string-cat-err-0.p4
##### spec-issue1297-string-cat-err-1.p4
```
;; Expr_ok/bine-plus-minus-mult:
(EL) -- if (exprIL_l'', exprIL_r'') = $reduce_senums_binary(exprIL_l', exprIL_r', def $compatible_plusminusmult)
```
- err-0: cannot add string and string
- err-1: cannot add string and int
Involves SpecTec function arguments.

##### issue1059.p4
##### issue1557-bmv2.p4
##### issue306.p4

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

##### issue3611.p4
##### issue3622.p4
```
;; Switch_tbl_label_ok/exprl:
(EL)  p C id_t b_last |- ExprL (NameE (CURRENT id_a)) : ExprL (NameE (CURRENT id_a) `(typ_e; ctk_e))
(IL) -- if (expr = `NameE%`_expr(`CURRENT%`_name(id_a))) ;;case expression 
(IL) -- if (expr =/= `NameE%`_expr(`CURRENT%`_name(id_a))) 
;; Switch_tbl_label_ok/defaultl:
(EL) p C id_t true |- DefaultL : DefaultL
```

Switch label must be an action name or `default`. Determined by case analysis with `NameE`.

##### duplicate-label.p4

```
;; Stmt_ok/switchs-gen
(EL) -- if $distinct_<switchlabel>(switchlabel*)
(IL) -- if $distinct_<switchlabel>(switchlabel*{switchlabel <- switchlabel*})
(CF) -- if ~$distinct_<switchlabel>(switchlabel*{switchlabel <- switchlabel*})
```

Switch labels must be distinct.

##### issue3299.p4
```
;; Stmt_ok/switchs-tbl, Stmt_ok/switchs-gen
(EL) -- if C.LOCAL.KIND = CONTROLAPPLYMETHOD
(IL) -- if C.LOCAL.KIND = `CONTROLAPPLYMETHOD`_lkind()
(CF) -- if C.LOCAL.KIND =/= `CONTROLAPPLYMETHOD`_lkind()
```
Switch statements can only be used within a control apply block.

##### issue774-1.p4
```
;; Call_convention_arg_ok/anya:
(EL) p C actctxt |- (_ OUT _ _) ~~ (AnyA, _) : AnyA
(IL) -- if (dir = `OUT`_dir())
(CF) -- if (dir =/= `OUT`_dir())
```
Don't care arguments can only be used when the direction is `OUT`.

### (3) Declaration

##### persistent_e.p4
##### push_nonconstant.p4
```
;; Call_convention_ok/no-not-action
(EL) -- if ctk = LCTK \/ ctk = CTK
(IL) -- if ((ctk = `LCTK`_ctk()) \/ (ctk = `CTK`_ctk()))
(CF) -- if ((ctk =/= `LCTK`_ctk()) /\ (ctk =/= `CTK`_ctk()))
```
Arguments to a non-action must be local compile-time known or compile-time known. 
- persistent_e: Arguments to an instantiation (non-action) 
- push_nonconstant: Arguments to built-in method `push_front`

##### accept_e.p4

```
;; Decl_ok/parserd
(EL) -- if ~("accept" <- statelabel*)
(IL) -- if ~ "accept" <- statelabel*{statelabel <- statelabel*}
(CF) -- if "accept" <- statelabel*
```

An "accept" state is reserved.

##### issue3388.p4
##### nostart.p4
```
;; Decl_ok/parserd:
(EL) -- if "start" <- statelabel'*
(IL) -- if $in_set<statelabel>("start", `SET%`_set<statelabel>(statelabel*{statelabel <- statelabel*}))
(CF) -- if ~$in_set<statelabel>("start", `SET%`_set<statelabel>(statelabel*{statelabel <- statelabel*}))
```
Parsers must have a `start` state.

##### issue3808-1.p4
##### issue819.p4
```
;; Decl_ok/controld:
(EL) (Param_ok: LOCAL C_2 |- param : paramIL eps)*
(IL) -- if (tid*' = [])
(CF) -- if (tid*' =/= [])
```
Controls cannot have implicit type parameters.

##### const_e.p4

```
;; Decl_ok/constd
(EL) -- if `(_; LCTK) = $annot(exprIL)
(IL) -- if (ctk = `LCTK`_ctk())
(CF) -- if (ctk =/= `LCTK`_ctk())
     -- i.e., ((ctk = `CTK`_ctk()) \/ (ctk = `DYN`_ctk()))
```

Trying to define a non local-compile time konwn value as constant.

##### missing_actions.p4
```
;; Decl_ok/tabled:
(EL) -- if |$actions_of_table(tbl)| = 1
(IL) -- if (|$actions_of_table(tbl)| = 1)
(CF) -- if (|$actions_of_table(tbl)| =/= 1)
```
Tables should have one action property

##### constructor_e.p4
```
;; Method_ok/externm
(EL) C_0 id_e |- ExternM ...
(IL) -- if (id_e =/= id)
(CF) -- if (id_e = id)
```

Extern constructors should not have return types.
Extern methods should not have the same name as the extern object being defined.

##### issue435.p4
```
;; Method_ok/externconsm:
(EL) C id_e |- ExternConsM id_e cparam* : C' (ExternConsM id_e tparam_hidden* cparamIL*)
(IL) -- if (id_e = id)
(CF) -- if (id_e =/= id)
```

Extern methods should have return types.

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
##### issue3359.p4

```
;; Decl_ok/vard-some
(EL) -- if $is_assignable(typ)
(IL) -- if $is_assignable(typ)
(CF) -- if ~$is_assignable(typ)
```

- issue2335: Trying to declare a variable of type `int`, which is illegal.
- issue3359: Trying to declare a variable of type `parser`, which is illegal.

##### issue394.p4
```
;; Deck_ok/vard-none
(EL) -- if $is_assignable(typ)
(IL) -- if $is_assignable(typ)
(CF) -- if ~$is_assignable(typ)
```
Trying to declare a variable of type extern object without instantiation, which is illegal.

## Y. Identified but Unclassified
##### issue774-2.p4
```
;; $resolve_cstrs()
(EL) -- if typ_h =/= AnyT
(IL) -- if (typ_h =/= ((typ) `AnyT`_synthtyp()))
(CF) -- if (typ_h = ((typ) `AnyT`_synthtyp()))
```
Don't care types cannot be used as arguments to generic method.

##### methodArgDontCare.p4 (?)
```
;; $resolve_cstrs()
(EL) $resolve_cstrs(MAP ((PAIR tid_h typ_h) :: (PAIR tid_t typ_t?)*))
  = $add_map<tid, typ>(MAP (PAIR tid_t' typ_t')*, tid_h, typ_h)
(IL) -- if (map<tid, typ?> = `MAP%`_map<tid, typ?>([]))
(CF) ??
```
Don't care types cannot be used as type arguments.

##### issue3170.p4
##### issue3293.p4
##### issue3305.p4
##### issue819-1.p4
```
;; $specialize_typedef
(EL) -- if theta = MAP (PAIR tparam' typ_a)*
```

Arity mismatch for polymorphic types. 

##### issue3656.p4
```
(EL) def $specialize_funcdef(MonoFD ft, eps) = (ft, eps)
(IL) -- if (typ* = [])
(CF) -- if (typ* =/= [])
```
Type parameters not allowed for action invocation.

##### issue3210.p4
##### issue3271-1.p4
##### issue3271.p4

Instantiation cannot be in a function.
Inaccurate p4spec behavior:
Currently handled as a parser error, so no premise is matched.

##### issue3282.p4
```
;; Expr_ok/inste
(EL) -- if $not_abstract_extern_object(typ)
(IL) -- if $not_abstract_extern_object(typ)
(CF) -- if ~$not_abstract_extern_object(typ)
```

Cannot instantiate abstract externs within an expression.

##### issue3246.p4
```
;; Expr_ok/callmethode_1
(EL) -- if ~$in_set<member>(member, SET ([ "minSizeInBits", "minSizeInBytes", "maxSizeInBits", "maxSizeInBytes" ]))
(CF) -- if $in_set<member>(member, SET ([ "minSizeInBits", "minSizeInBytes", "maxSizeInBits", "maxSizeInBytes" ]))
```
Type arguments supplied for built-in member calls. 

##### issue3335.p4
##### pr2918.p4
##### select-struct.p4
##### select-varbits.p4
```
;; Expr_ok/selecte:
;; Type_wf/sett:
(EL) -- if $nestable_sett(typ)
(CF) -- if ~$nestable_sett(typ)

```

- issue3335: Int cannot be used as a select key.
- pr2918, select-struct: Struct cannot be used as a select key.
- select-varbits: `varbit<12>` cannot be used as a select key.

##### issue3751.p4
```
;; Decl_ok/valuesetd:
(EL) -- if ctk = CTK \/ ctk = LCTK
(IL) -- if ((ctk = `CTK`_ctk()) \/ (ctk = `LCTK`_ctk()))
(CF) -- if ((ctk =/= `CTK`_ctk()) /\ (ctk =/= `LCTK`_ctk()))
```
Type argument of `value_set` must be local compile-time known or compile-time known.

##### issue3346.p4
```
;; Decl_ok/valuesetd:
(EL) -- Type_wf: $bound_tids(p, C) |- SetT typ_s
;; Type_wf/sett:
(EL) -- if $nestable_sett(typ)
(CF) -- if ~$nestable_sett(typ)
```
Inaccurate p4spec behavior:
`valuesetd` should only take types of `FIntT` and `FBitT`.
However, CF sufficiently covers the issue.

##### issue3285.p4
Width of signed int cannot be zero.
Inaccurate p4spec behavior:
Currently handled as transform error, so no premise is matched.

##### issue3363.p4
```
;; Expr_ok/bine-compare:
(EL) -- if (exprIL_l'', exprIL_r'') = $reduce_senums_binary(exprIL_l', exprIL_r', def $compatible_compare)
```

Externs cannot be compared.
Inaccurate p4spec behavior:
Currently passes typechecker. The above is where the typechecker is supposed to fail.

##### issue4140.p4
##### issue4142.p4 
```
;; Decl_ok/instd-none:
```

`main` is reserved for package instances.
- issue4140: Tries to instantiate an `extern` as `main`.
Inaccurate p4spec behavior:
Currently passes typechecker. There should be a premise for `id` in `Decl_ok/instd-none`.
- issue4142: Tries to use `bit<32>` as type for `main`.
Inaccurate p4spec behavior:
Currently handled as transform error.

##### issue3589.p4
```
;; Expr_ok/bine-compare:
(EL) -- if (exprIL_l'', exprIL_r'') = $reduce_senums_binary(exprIL_l', exprIL_r', def $compatible_compare)
```
Tables cannot be compared.

##### issue3846.p4
```
;; Expr_ok/bine-compare-eq:
(EL) -- if $is_equalable(typ')
(IL) -- if $is_equalable(typ')
(CF) -- if ~$is_equalable(typ')
```
- issue3846: Table application results cannot be compared of equality.
- issue3847: Table action lists cannot be compared of equality.

##### issue345.p4
Control types cannot be generic.
Inaccurate p4spec behavior:
The generic type is interpreted as a free variable and fails at `$find_map`.

##### loop1-err.p4
##### loop2-err.p4
##### loop3-err.p4
##### minWidth.p4
##### p_e.p4
##### shift_e.p4
Currently handled as parser error.

## Z. Other Unclassified
- action-named-default-bmv2.p4
- constructor1_e.p4
- constructor2_e.p4
- control-inline.p4
- default-initializerierr.p4
- dup-param1.p4
- dup-param2.p4
- enumCast.p4
- expression_e.p4
- factory-err2.p4
- generic1_e.p4
- globalVar_e.p4
- header1_e.p4
- invalid-invalid.p4
- issue1202.p4
- issue1336.p4
- issue1777-bmv2.p4
- issue2441.p4
- issue2544_recursive.p4
- [ ] issue2835-bmv2.p4
- [ ] stack1_e.p4
- [ ] stack2.p4
- [ ] stack3.p4
- [ ] stack_e.p4
- [ ] string-e.p4
- [ ] string-e1.p4
- [ ] string-e2.p4
- [ ] struct_e.p4
- [ ] structure-valued-expr-errs-1.p4
- [ ] structure-valued-expr-errs-2.p4
- [ ] structured-annotation-e.p4
- [ ] table-entries-decl-order.p4
- [ ] table-entries-exact-ternary.p4
- [ ] table-entries-exact.p4
- [ ] table-entries-lpm-2.p4
- [ ] table-entries-lpm.p4
- [ ] table-entries-optional-2-bmv2.p4
- [ ] table-entries-outside-table.p4
- [ ] table-entries-range.p4
- [ ] table-entries-ternary.p4
- [ ] template_e.p4
- [ ] tuple-left.p4
- [ ] tuple-newtype.p4
- [ ] tuple-to-header.p4
- [ ] type-field.p4
- [ ] type-in-expr-lex0.p4
- [ ] type-in-expr-lex1.p4
- [ ] type-in-expr.p4
- [ ] type-params_e.p4
- [ ] typecheck1_e.p4
- [ ] typecheck_e.p4
- [ ] typedef-and-type-definitions.p4
- [ ] underscore1_e.p4
- [ ] underscore2_e.p4
- [ ] underscore3_e.p4
- [ ] underscore_e.p4
- [ ] virtual1.p4
- [ ] virtual2.p4
- [ ] virtual3.p4
- [ ] virtual4.p4
- [ ] virtual5.p4
- [ ] virtual6.p4
- [ ] wrong-cast.p4
