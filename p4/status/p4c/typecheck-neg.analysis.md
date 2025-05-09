# A. Fix Logic

## \[DONE\] 1. ~~Function definition and calls~~

Restricting function well-formedness (parameter types and return types) and valid call-sites.

### \[DONE\] (1) ~~Function well-formedness~~

e.g., action should not take an `int` parameter.

```p4
action a(int x) {}
```

Also, type inference may specialize an ill-formed function.

```p4
control C() { apply { } }
void f<T>(T t) {}
control D() {
    C() c;
    apply { f(c); }
}
```

For actions, the spec mentions:

> Action parameters that have no direction (e.g., port in the previous example) indicate “action data.” All such parameters must appear at the end of the parameter list. (14.1)

```p4
action a(bit x0, out bit y0) {
    bit x = x0;
    y0 = x0 & x;
}
```

### \[DONE\] (2) ~~Constructor call-sites~~

Relating to valid call-sites,

```p4
package myp(bit a);
void func() {
    myp(1w1) a;
}
```

### \[DONE\] (3) ~~Function call-sites~~

Relating to valid call-sites,

```p4
control c() {
    action a() {}
    table t {
        actions = { a(); }
        default_action = a();
    }
    action b() {
        t.apply();  // cannot invoke table from action
    }
    apply {}
}
```

## \[DONE\] 2. ~~Duplicate declarations in the same namespace~~

### \[DONE\] (1) ~~Duplicate id~~

```p4
control c(bit<32> p)(bool p) {
  apply {}
}
```

### \[DONE\] (2) ~~Duplicate constants~~

```p4
const bit<4> a = 1;
const bit<4> a = 2;
```

### \[DONE\] (3) ~~Duplicate action/functions~~

```p4
action foo (in bit<8> x, out bit<8> y) { y = (x >> 2); }
action foo (inout bit<8> x) { x = (x >> 3); }
```

### \[DONE\] (4) ~~Duplicate switch label~~

```p4
apply {
    switch (t.apply().action_run) {
        a: { arun = 1; }
        a: { arun = 1; }  // duplicate label
    }
}
```

## \[DONE\] 3. ~~Table property~~

### \[DONE\] (1) ~~Missing table action property~~

```p4
table t {
    key = { x : exact; }
}
```

### \[DONE\] (2) ~~Specifying table entry with empty key~~

```p4
table t {
    key = {}
    actions = { a; }
    const entries = {
      _ : a();
    }
}
```

### \[DONE\] (3) ~~Table properties are sensitive to the order of declaration~~

```p4
table badtable {
    default_action = nop;
    actions = { nop; }
}

apply {
    badtable.apply();
}
```

### \[DONE\] (4) ~~Bad mask for longest prefix match~~

```p4
// value has 1s outside of field bit width
0x100 &&& 0xF0 : a_with_control_params(11);

// mask has 1s outside of field bit width
0x11 &&& 0x1F0 : a_with_control_params(12);

// mask that is not a prefix mask
0x11 &&& 0xE1 : a_with_control_params(13);

// another mask that is not a prefix mask, and has 1 bits
// outside of field bit width.
0x11 &&& 0x181 : a_with_control_params(14);

// exact match value with value having 1s outside of field
// bit width
0x100 : a_with_control_params(15);
```

## \[DONE\] 4. ~~Compile-time evaluation~~

### \[DONE\] (1) ~~Stack bounds check if compile-time known~~

```p4
// Compiler should give error for out of bounds index,
// if due to compile-time known value index.
h.hs[-1].setValid();
h.hs[-1].f1 = 5;
h.hs[-1].f2 = 8;

// No error or warning for these -- they are good.
h.hs[0].setValid();
h.hs[0].f1 = 5;
h.hs[0].f2 = 8;
h.hs[2].setValid();
h.hs[2].f1 = 5;
h.hs[2].f2 = 8;

// Compiler should give error for out of bounds index,
// if due to compile-time known value index.
h.hs[3].setValid();
h.hs[3].f1 = 5;
h.hs[3].f2 = 8;
```

### \[DONE\] (2) ~~Bitslice width check~~

```p4
bit<8> n = 8w0b11111111;
n[7:4][5:2] = 4w0;
```

### \[DONE\] (3) ~~Division or modulo with negative value~~

P4 forbids division or modulo with negative values.
But the question is, can we guarantee this at compile time?

```p4
action act() {
    bit<8> a;
    a = - 8 / -2;
    a = -10 % 2;
    a = 10 % -2;
}
```

## \[DONE\] 5. Context-sensitivity (some impose implicit domain-specific P4 knowledge)

### \[DONE\] (1) ~~`.last`, `.lastIndex` only allowed within parser~~

The spec mentions:

> * hs.last: produces a reference to the element with index hs.nextIndex - 1 in the stack, if such an element exists. May only be used in a parser.
> * hs.lastIndex: produces a 32-bit unsigned integer that encodes the index hs.nextIndex - 1. May only be used in a parser. (8.18)

```p4
action a(in s v) {
    bit<32> t = v.field.lastIndex;
}
```

```
control c() {
    h[10] stack;

    apply {
        stack.last = { 10 };
    }
}
```

### \[DONE\] (2) ~~Calling `verify` within a control~~

The spec mentions:

> The verify statement provides a simple form of error handling. verify can only be invoked within a parser; (13.7)

~~But this is specific to P4 invariant. Not something that can be inferred from the code itself.~~

Actually, should have modeled `verify` as a dedicated AST node.
But `verify` exists as an extern function, supposedly for backward-compatibility reasons?

```p4
// TODO: remove from this file, convert to built-in
/// Check a predicate @check in the parser; if the predicate is true do nothing,
/// otherwise set the parser error to @toSignal, and transition to the `reject` state.
extern void verify(in bool check, in error toSignal);
```

```p4
control C() {
  apply {
    verify(8w0 == 8w1, error.Oops);
   }
}
```

### \[DONE\] (3) ~~No switch inside action~~

~~But maybe this is only specific to `p4test`.~~

The spec mentions:

> The switch statement can only be used within control blocks. (12.7)

Yet, it does seem to impose a stronger restriction that switch can only be used within the apply block of a control.

```p4
action a0() {
	switch (m.m0) {
	    8w0x0: {}
	}
}
```

### \[DONE\] (4) ~~Variable `main` should be a package instance~~

```p4
extern MyExtern {
   MyExtern();
}
MyExtern() main;
```

## \[DONE\] 6. ~~Devils are in the details~~

### \[DONE\] (1) ~~Type inference should fail~~

```p4
b.extract(_);
```

Type inference fails if we cannot infer a concrete type for the don't care argument.

### \[DONE\] (2) ~~Table invocation results are incomparable~~

```p4
table tt {
    actions = {}
}
apply {
    if (tt.apply() == tt.apply()) {}
}
```

```p4
if (tt.apply().action_run == tt.apply().action_run)
```

The spec mentions:

> table.apply().action_run, which can only appear as the expression of a switch statement (see Section 12.7), and when it appears there, it must be the entire expression. (12.5)
> Any expression containing table.apply().hit or table.apply().miss (see Section 14.2.2), which can be part of arbitrarily complex expressions in many places of a P4 program. (12.5)

### \[DONE\] (3) ~~Nesting `int` inside a value set~~

```p4
value_set<int>(4) myvs;
```

### \[DONE\] (4) ~~Implicit cast omitting an intermediate step~~

```p4
typedef bit<1> b1;
typedef bit<2> b2;
type b1 t1;
type b2 t2;

t1 func(b2 a) {
  return (t1)a;
}
```

Here, we are trying to cast `bit<2>` to `t1`, which is a new type for `bit<1>`. But we *cannot* implicitly cast `bit<2>` to `bit<1>`.

### \[DONE\] (5) ~~Parser swallows the unary `+`~~

```ocaml
| info1 = PLUS exp = expression %prec PREFIX
    { (*let info2,exp = exp in*)
      let tags = Source.merge info1 (Expression.tags exp) in
      Expression.update_tags exp tags }
```

So the below program is checked as correct.

```p4
const bool b = +false;
```

### \[DONE\] (6) ~~Instantiation without abstract method~~

```p4
extern g {
  g();
  abstract void h();
}
package p(g a);
p(g()) main;
```

### \[DONE\] (7) ~~Method with same name as object~~

```p4
extern X {
    void X();
}
```

### \[DONE\] (8) ~~Table application is disallowed in action argument~~

The spec mentions:

> Applying tables, whether directly via an expression like table1.apply().hit, or indirectly, are forbidden in the expressions supplied as action arguments. (14.2.1.2)

But not sure how to enforce this.

Solved by introducing an AST walker, though it is inefficient.

```p4
a_two(
    t_two.apply().hit ?
        bump_val(hdr.ethernet.etherType) :
        hdr.ethernet.etherType,
    bump_val(hdr.ethernet.etherType));
```

# B. Need Spec Clarification

## \[REPORTED\] 1. Test too strict on explicit cast: [cast-to-struct](../testdata/p4c/program/ill-typed-excluded/spec-clarify/cast-to-struct)

Waiting for spec clarification, [Issue#1351](https://github.com/p4lang/p4-spec/issues/1351).
And a [Related Discussion](https://github.com/p4lang/p4c/issues/3233) in the p4c repo.

```p4
struct s {
  bit t;
  bit t1;
}

s func(bit t, bit t1) {
  return (s)(s)(s){t, t1};
}
```

Rationale for disallowing this: the P4 spec does not allow explicit cast to a struct type, with one exception, when initializing a struct variable.
But maybe this is too strict?

## \[REPORTED\] 2. Restrictions on constructor invocation sites

Waiting for spec clarification, [Issue#1349](https://github.com/p4lang/p4-spec/issues/1349).

The spec puts restrictions on constructor invocation sites in Appendix F.
Specifically for the top-level,

```plaintext
can be instantiated in this place
This type       | top level
package         | yes
parser          | no 
control         | no 
extern          | yes
function        | yes
table           | no 
value-set       | yes
value types     | N/A
```

Yet, parsers and controls can be instantiated in the top-level, when they are used as constructor arguments to a package instantiation.
So the spec should be more precise on this matter.
p4cherry adjusts the instantiation site for constructor arguments as package-local when instantiating a package at the top-level scope.

# C. Need Test Clarification

## 1. Comparison between sequence and record types: [compare-sequence-record](../testdata/p4c/program/ill-typed-excluded/test-clarify/compare-sequence-record)

```p4
bool b1 = { 1, 2 } == { 1, 2 };
bool b2 = { a = 32w1, b = 32w2 } == { a = 32w1, b = 32w2 };
bool b2_ = { a = 1,b = 2 } == { a = 1, b = 2 };
```

I think this should be allowed.

## 2. Too strict for indirect recursive type?: [self-nesting-struct](../testdata/p4c/program/ill-typed-excluded/test-clarify/self-nesting-struct)

```p4
struct h<t>{
  t f;
}
typedef h<bit> tt;
typedef h<tt> t;
```

## 3. Struct parameter not allowed for an action?: [action-struct-param](../testdata/p4c/program/ill-typed-excluded/test-clarify/action-struct-param)

```p4
struct choices_t {
  s1_t entry0;
  s1_t entry1;
  s1_t entry2;
  s1_t entry3;
}
...
action select_entry(choices_t choices) { ... }
```

## 4. Variables, type varaibles, constructors, and functions live in the same namespace?: [namespace-var-tvar-func-ctor](../testdata/p4c/program/ill-typed-excluded/test-clarify/namespace-var-tvar-func-ctor)

```p4
control foo (in bit<8> x, out bit<8> y) { apply { y = x + 7; } }
bool foo() { return true; }
```

p4c rejects this program because:

```
p4c/testdata/p4_16_errors/issue1932.p4(2): [--Werror=duplicate] error: Re-declaration of foo with different type:
bool foo(
     ^^^
p4c/testdata/p4_16_errors/issue1932.p4(1)
control foo (in bit<8> x, out bit<8> y) { apply { y = x + 7; } }
        ^^^
[--Werror=overlimit] error: 1 errors encountered, aborting compilation
```

But we can distinguish the use of an identifier as a function or a constructor from the syntax. So we may consider them as living in different namespaces, so the above program should (or can) be accepted.

Similarly for varaibles and functions: `main` is reserved for the top-level package instance.
But is `main` allowed for type variables, constructors, and functions?
p4cherry allows such and p4c disallows such as below.

```p4
extern main {}
```

Here, `main` is a constructor name, not a variable name.

## \[REPORTED\] 5. Scope of a control parameter: [control-shadowing](../testdata/p4c/program/ill-typed-excluded/test-clarify/control-shadowing)

Reported to p4c, [Issue#5092](https://github.com/p4lang/p4c/issues/5092).

What is the scope of a control parameter?
Does it live in the same level as the local declarations, or does it live in the same level as the `apply` block?

```p4
control ingress(inout Headers h) {
    apply {
        Headers h = h;
    }
}
```

The above case is categorized as ill-typed, by the p4c compiler.
But the below case passes the type check.

```p4
control ingress(inout Headers h) {
    Headers h = h;
    apply {}
}
```

They imply that the control parameter is in the same level as the `apply` block, but not in the same level as the control local declarations.

So it suggests two cases:

(i) Local declarations are above the level of control parameters and `apply` block. (`local` > `control parameter` = `apply`)

This way, we are viewing

```p4
control ingress(inout Headers h) {
    Headers local = h;
    apply {
        Headers app = local;
    }
}
```

as,

```p4
control ingress {
    Headers local;
    apply (inout Headers h) {
        local = h;
        Headers app = local;
    }
}
```

This gives one explanation for the above case.
However, this will no longer justify the below case.

```p4
control ingress(inout Headers h) { // 1. if we move this to apply,
    Headers local = h;
    action a() {
        Headers act = h; // 2. error: h is not defined
    }
    apply {
        Headers app = local;
    }
}
```

Adding more to its strangeness,

```p4
control C(inout bit<32> x) {
   action a() { bit<32> x = x; }
   apply {}
}
```

```
dup.p4(7): [--Werror=shadow] error: declaration of 'x' shadows a parameter 'x'
   action a() { bit<32> x = x; }
                ^^^^^^^^^^^^^
dup.p4(6)
control C(bit<32> x) {
```

```p4
control C(inout bit<32> x) {
   bit<32> x = 3;
   action a() { bit<32> x = x; }
   apply {}
}
```

```
dup.p4(7): [--Wwarn=shadow] warning: 'x' shadows 'x'
   bit<32> x = 3;
   ^^^^^^^^^^^^^
dup.p4(6)
control C(bit<32> x) {
                  ^
dup.p4(8): [--Wwarn=shadow] warning: 'x' shadows 'x'
   action a() { bit<32> x = x; }
                ^^^^^^^^^^^^^
dup.p4(7)
   bit<32> x = 3;
   ^^^^^^^^^^^^^
```

above is rejected and below is accepted. (above implies that `param` = `apply` and below implies that `param` > `local` > `apply`)

(ii) Local declarations are below the level of control parameters and `apply` block. (`local` < `control parameter` = `apply`)

This is not true because the `apply` block can access the local declarations.

## \[REPORTED\] 6. Type inference for `int`: [type-inference-return-int](../testdata/p4c/program/ill-typed-excluded/test-clarify/type-inference-return-int)

Reported to p4c, [Issue#5090](https://github.com/p4lang/p4c/issues/5090).

```p4
T f<T>(T x) {
    return x;
}
...
bit<8> y = f(255);
```

This should type check with `T` as `int`. (It is also well-formed since `int` is a directionless parameter.)
But the p4c compiler rejects this program with the following error:

```
issue2260-1.p4(8): [--Werror=type-error] error: 'f(255)'
        bit<8> y = f(255);
                   ^^^^^^
  ---- Actual error:
  'int' type can only be unified with 'int', 'bit<>', or 'signed<>' types, not with '<returned type>'
  ---- Originating from:
issue2260-1.p4(3): Return type 'T' cannot be used for '<returned type>'
  T f<T>(T x) {
      ^
  Where 'T' is bound to 'int'
  ---- Originating from:
issue2260-1.p4(3): Function type 'f' does not match invocation type '<Method call>'
  T f<T>(T x) {
    ^
issue2260-1.p4(8)
          bit<8> y = f(255);
                     ^^^^^^
```

## 7. Returning `int` from a method is illegal?: [method-int-return](../testdata/p4c/program/ill-typed-excluded/test-clarify/method-int-return)

```p4
extern e {
    e();
    abstract int f();
}

e() t = {
    int f() { return 1; }
};
```

p4c rejects this program with the following error:

```
issue3273.p4(3): [--Werror=type-error] error: int: illegal return type for method
    abstract int f();
             ^^^
```

However, the spec does not mandate this.

## \[REPORTED\] 8. Shifting an arbitrary precision integer: [shift-int-by-fixed](../testdata/p4c/program/ill-typed-excluded/test-clarify/shift-int-by-fixed)

Reported to p4c, [Issue#5091](https://github.com/p4lang/p4c/issues/5091).

Below tests expect that when an arbitrary precision integer is shifted either left or right, the right operand should be a compile-time known value.

However, the spec says, for arbitrary precision integers:

> Arithmetic shift left and right denoted by << and >>. These operations produce an int result. The right operand must be either an unsigned value of type bit\<S\> or a compile-time known value that is a non-negative integer. (8.8)

i.e., if the right operand is `bit<S>`, then it need not be a compile-time known value.

```p4
header H {
    bit<8> a;
    bit<8> b;
    bit<8> c;
}
struct Headers {
    H h;
}
control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    apply {
        h.h.a = (1 << h.h.c) + 8w2;
    }
}
```

```p4
issue2206.p4(27): [--Werror=type-error] error: 1 << h.h.c: shift result type is arbitrary-precision int, but right operand is not constant; width of left operand of shift needs to be specified or both operands need to be constant
        h.h.a = (1 << h.h.c) + 8w2;
                 ^^^^^^^^^^
```

```p4
const int a = 5;
hdr.v = (bit<8>)(a >> b);
```

## 9. Calling an extern with nested struct argument is not supported: [extern-nested-struct-param](../testdata/p4c/program/ill-typed-excluded/test-clarify/extern-nested-struct-param)

p4c disallows calling an extern function with a nested struct argument with `out` direction.

This is overly restrictive, as mentioned in this [Issue#2545](https://github.com/p4lang/p4c/issues/2545).

> This is an interesting case. p4test contains a pass which indeed does not support extern functions with out arguments that are nested structs. This pass is usable as a mid-end pass for p4c-bm2-ss, but it is not a general-purpose pass. I will improve the compiler to give an error for this case rather than generating an illegal program. This is a case where p4test is too restrictive; it does not handle arbitrary legal p4 programs.

```p4
extern bit<64> call_extern(inout Headers val);

control ingress(inout Headers h) {
    apply {
        Headers tmp = h;
        call_extern(tmp);
        h = tmp;
    }
}
```

```plaintext
issue2545.p4(17): [--Werror=target-error] error: call_extern({ eth_hdr = tmp_0_eth_hdr }): extern functions with 'out' nested struct argument (val) not supported
        call_extern(tmp);
        ^^^^^^^^^^^^^^^^
```

## \[REPORTED\] 10. Directionless action arguments in a program acts like `in`?: [directionless-action-arg-as-in](../testdata/p4c/program/ill-typed-excluded/test-clarify/directionless-action-arg-as-in)

Waiting for test clarification, [Issue#5042](https://github.com/p4lang/p4c/issues/5042).

~~Waiting for spec clarification, [Issue#1350](https://github.com/p4lang/p4-spec/issues/1350).~~

~~Partly yes, because they can be implicitly cast (ongoing PR exists). Partly no, because they must be compile-time known.~~

```p4
action b(inout bit<32> x, bit<8> data) {
    meta.a = meta.a ^ (bit<4>) x ^ (bit<4>) data;
}
table t1 {
    key = { hdr.ethernet.srcAddr : exact; }
    actions = { b(meta.c); }
    default_action = b(meta.c, (bit<8>) meta.d);
}
```

# D. More than a type check? (Requiring domain-specific knowledge)

## 1. Semantics of packet extraction: [packet-extract](../testdata/p4c/program/ill-typed-excluded/over-typecheck/packet-extract)

Some tests impose implicit (at least in the code syntax level) restrictions.

```p4
parser P(packet_in p, out bit<32> h) {
    state start {
        p.extract(h);  // error: not a header
        transition accept;
    }
}
```

```p4
parser P(packet_in p, out H h) {
    state start {
        p.extract(h, 32);  // error: not a variable-sized header
        transition accept;
    }
}
```

```p4
struct my_packet {
    h_t[10] h;
}
parser MyParser(packet_in b, out my_packet p) {
    state start {
        b.extract(p.h);  // error: not a header
        transition accept;
    }
}
```

```p4
header h_t {
    bit<8> f;
    varbit<8> g;
}
struct my_packet {
    h_t h;
}
parser MyParser(packet_in b, out my_packet p) {
    state start {
        b.extract(p.h);  // error: variable-sized header
        transition accept;
    }
}
```

## 2. Semantics of packet lookahead: [packet-lookahead](../testdata/p4c/program/ill-typed-excluded/over-typecheck/packet-lookahead)

```p4
packet.lookahead<void>();
```

```p4
header H {
    varbit<120> x;
}
...
h = pkt.lookahead<H>();
```

## 3. Reachability analysis of parser state machine: [parser-state-reachability](../testdata/p4c/program/ill-typed-excluded/over-typecheck/parser-state-reachability)

The test case implies a constraint that the parser state machine should terminate in either `accept` or `reject` state.
But it is not mentioned in the specification.
Also it requires unrolling of parser states, which will not be performed in p4cherry, since p4cherry is a P4 interpreter, not a compiler.


```p4
state start {
    transition state_1;
}
state state_1 {
    transition state_2;
}
state state_2 {
    transition state_3;
}
state state_3 {
    transition state_2;
}
```

# E. Unsupported

## 1. Practical concerns: using `exact` match kind internally in a `switch` implementation: [switch-internal-match-kind](../testdata/p4c/program/ill-typed-excluded/unsupported/switch-internal-match-kind)

```p4
control c(in bit<4> a) {
  apply {
    switch (a) {
      4w0: {}
    }
  }
}
```

```
[--Werror=not-found] error: Could not find declaration for 'match_kind.exact', which is needed to implement switch statements; did you include core.p4?
```

But this is unnecessary for p4cherry.

## 2. Practical concerns: large number: [large-number](../testdata/p4c/program/ill-typed-excluded/unsupported/large-number)

```p4
// this expression will slow the compiler to a crawl to print a warning
h.eth_hdr.eth_type =  1985245330 << 903012108;
```

This is irrelevant for p4cherry.

## 3. Annotation: [annotation](../testdata/p4c/program/ill-typed-excluded/unsupported/annotation)

```p4
@pkginfo
const bit<32> x = 0;
```

```p4
table t1 {
    key = {
        hdr.ethernet.etherType: exact;
    }
    actions = {
        @tableonly a1;
        a2;
        @defaultonly a3;
    }
    const entries = {
        // Ideally the following line should cause an error during
        // compilation because action a3 is annotated @defaultonly
        3 : a3();
    }
    // Ideally the following line should cause an error during
    // compilation because action a1 is annotated @tableonly
    default_action = a1;
}
```

```p4
@match(1+1) bit<16> f16;
```

```p4
@name(".t0")
table t0 {
    key = { smeta.ingress_port : exact; }
    actions = { drop; NoAction; }
    const default_action = NoAction();
}
```

```p4
key = { h.a + h.a : exact; }
```

```
key-name.p4(28): [--Werror=expected] error: h.a + h.a: Complex key expression requires a @name annotation
        key = { h.a + h.a : exact; }
                ^^^^^^^^^
```

```p4
header Hdr {
    varbit<256> data0;
    @length(data0) // illegal: expression is not uint<32>
    varbit<12> data1;
    @length(size2) // illegal: cannot use size2, defined after data2
    varbit<256> data2;
    int<32> size2;
}
```

## 4. Target-specific: select cases should be compile-time known?: [select-case-compile-time-known](../testdata/p4c/program/ill-typed-excluded/unsupported/select-case-compile-time-known)

The spec mentions:

> Some targets may require that all keyset expressions in a select expression be compile-time known values. (13.6)

Current p4cherry does not (and probably will not) enforce this.

```p4
transition select(hdr.ethernet.etherType) {
    0x0806 .. 0x0800 : parse_ipv4;
    2054 .. 2048 : parse_ipv4;
    hdr.ipv4.totalLen .. 0x0800 : parse_ipv4;
    0x0800 .. hdr.ipv4.totalLen : parse_ipv4;
    default: accept;
}
```

```p4
transition select(hdr.eth_hdr.eth_type) {
    simple_action(): reject;
    default: accept;
}
```

```p4
bit<8> x;
state start {
    transition select(8w0) {
        x &&& x: accept;
        default: reject;
    }
}
```

## 5. Target-specific: Already expecting `NoAction` even when core.p4 is not included [expect-noaction](../testdata/p4c/program/ill-typed-excluded/unsupported/expect-noaction)

```p4
action NoAction(bit t) {}
control c() {
    table t {
        actions = {}
    }
    apply {}
}
```

```
issue3644-1.p4(1): [--Werror=Target model error] error: NoAction: Expected an action with no parameters; did you include core.p4?
action NoAction(bit t) {}
       ^^^^^^^^
```

```p4
const bit NoAction = 1;
control c() {
    table t {
        actions = {}
    }
    apply {}
}
```
