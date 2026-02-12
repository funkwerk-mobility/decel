# decel — TODO

## ✅ Implemented

- [x] Lexer: full CEL token set (ints, uints, floats, strings, bytes, keywords, operators)
- [x] Pratt parser with correct operator precedence
- [x] Integer arithmetic (+, -, *, /, %)
- [x] Unsigned integer arithmetic
- [x] Double arithmetic (with int/uint promotion)
- [x] String concatenation and comparison
- [x] Boolean logic (&&, ||, !)
- [x] Short-circuit evaluation (false && err → false, true || err → true)
- [x] Error-value semantics (eval errors are Values, not exceptions)
- [x] Comparison operators (<, <=, >, >=, ==, !=)
- [x] Ternary conditional (? :)
- [x] List literals and indexing
- [x] Map literals (string keys), field access, and index access
- [x] Unary negation (-, !)
- [x] Variable resolution via Context
- [x] `in` operator (string contains, map key membership)
- [x] `size()` function and method
- [x] `.contains()`, `.startsWith()`, `.endsWith()` string methods
- [x] Type cast functions: `int()`, `uint()`, `double()`, `string()`
- [x] `type()` function
- [x] Entry (lazy map) support via abstract class
- [x] Nested member access (e.g., `req.method`)
- [x] List concatenation with `+`
- [x] Triple-quoted strings
- [x] Raw strings (r"...")
- [x] Hex integer literals (0xFF)
- [x] Escape sequences in strings

## 🔨 Core CEL Features — Not Yet Implemented

### High Priority

- [ ] **`has()` macro** — `has(x.y)` tests field existence without evaluating.
      Needs special parser handling: don't evaluate the argument, just check
      if the member/key exists. Returns `bool`.

- [ ] **`.matches()` regex method** — `s.matches(re)` for RE2-style regex.
      D's `std.regex` should work. Need to decide on caching compiled patterns.

- [ ] **`Value.opEquals` / deep equality** — The `in` operator on lists
      currently always returns `false` because we can't compare Values.
      Need `opEquals` on Value (recursive for lists/maps).

- [ ] **Null semantics** — CEL specifies: `null == null` is `true`,
      `null != X` is `true` for non-null X, arithmetic with null is an error.
      Verify and test our current behavior.

- [ ] **Duration and timestamp types** — CEL has `google.protobuf.Timestamp`
      and `google.protobuf.Duration` as first-class types with arithmetic.
      Could use D's `core.time.Duration` and `std.datetime.SysTime`.

### Medium Priority

- [ ] **Non-string map keys** — CEL spec allows `bool`, `int`, `uint`, and
      `string` as map keys. We only support `string`. Would need to change
      `Value[string]` to a custom map type. (See COMPATIBILITY.md)

- [ ] **Bytes operations** — Bytes literals are tokenized but bytes values
      aren't fully operational. Need: comparison, `size()`, `+` concatenation.

- [ ] **List/map equality** — `[1, 2] == [1, 2]` should be `true`.
      Requires recursive deep equality (blocked on `Value.opEquals`).

- [ ] **Unsigned/signed cross-type comparison** — `1u == 1` should be `true`.
      Currently uint and int are different types with no cross-comparison.

- [ ] **Overflow detection** — CEL specifies that integer overflow is an error.
      We currently wrap silently.

### Lower Priority

- [ ] **Comprehensions / macros** — `list.all(x, x > 0)`, `list.exists(x, x > 0)`,
      `list.map(x, x * 2)`, `list.filter(x, x > 0)`. These are macros in CEL,
      not functions — the second argument is an expression, not a value.

- [ ] **Custom functions** — User-registered functions via `Env`. Need a
      registration API and dispatch mechanism.

- [ ] **`dyn()` type** — Dynamic type assertion, rarely used in practice.

- [ ] **Enum support** — CEL can reference protobuf enum values by name.

- [ ] **Better error messages** — Include source context (the expression
      snippet) in error messages, not just byte offset.

- [ ] **Performance** — Currently re-tokenizes on every `evaluate()` call.
      Could cache token arrays or build a simple AST for repeated evaluation.

## 🏗️ Infrastructure

- [ ] **Conformance tests** — Run against the official
      [cel-spec conformance suite](https://github.com/google/cel-spec/tree/master/tests).

- [ ] **README with examples** — Usage documentation, API examples.

- [ ] **DUB package publishing** — Register on code.dlang.org.

- [ ] **Benchmarks** — Compare against cel-go for common expressions.

## Architecture Notes

- Single-pass tokenize → evaluate. No AST.
- Error values (`Value.err`) for evaluation errors (division by zero,
  type mismatches, missing keys). Errors propagate through operators
  and are naturally absorbed by short-circuit `&&` and `||`.
- `EvalException` only for parse/syntax errors (unexpected token, etc.).
- Pratt parsing for operator precedence.
- Short-circuit `&&` and `||` — both sides are always parsed, but
  `false && err` → `false` and `true || err` → `true`.