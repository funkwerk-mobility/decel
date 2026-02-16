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
- [x] Strict bool semantics (&&, ||, ternary reject non-bool operands)
- [x] Error-value semantics (eval errors are Values, not exceptions)
- [x] Comparison operators (<, <=, >, >=, ==, !=)
- [x] Ternary conditional (? :)
- [x] List literals and indexing
- [x] Map literals (string keys), field access, and index access
- [x] Unary negation (-, !)
- [x] Variable resolution via Context
- [x] `in` operator (string contains, list membership, map key membership)
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
- [x] `has()` macro
- [x] `Value.opEquals` / deep equality (lists, maps, all types)
- [x] Macro system (function-call and method-call macros)
- [x] Comprehensions: `.all()`, `.exists()`, `.exists_one()`, `.map()`, `.filter()`
- [x] `.matches()` regex method — `s.matches(re)` for full-string regex matching

## 🎯 1.0.0 Blockers

- [ ] **Null semantics** — verify and test: `null == null` → `true`,
      `null != X` → `true` for non-null X, arithmetic with null → error.
      Likely already works, just needs explicit test coverage.

- [ ] **Cross-type numeric comparison** — `1u == 1` should be `true`,
      `1u < 2` should work, `1.0 == 1` should be `true`. Currently
      uint and int are separate types with no cross-comparison.

- [ ] **Duration and timestamp types** — CEL has `google.protobuf.Timestamp`
      and `google.protobuf.Duration` as first-class types. Use D's
      `core.time.Duration` and `std.datetime.SysTime`. Support arithmetic
      (timestamp - timestamp → duration, timestamp + duration → timestamp),
      comparison, and `duration()`/`timestamp()` constructors.

- [ ] **README with examples** — Usage documentation, API examples,
      quick-start guide. Can't ship a library without docs.

## 🔮 Post-1.0

### Medium Priority

- [ ] **Non-string map keys** — CEL spec allows `bool`, `int`, `uint`, and
      `string` as map keys. We only support `string`. Would need to change
      `Value[string]` to a custom map type. (See COMPATIBILITY.md)

- [ ] **Bytes operations** — Bytes literals are tokenized but bytes values
      aren't fully operational. Need: comparison, `size()`, `+` concatenation.

- [ ] **Overflow detection** — CEL specifies that integer overflow is an error.
      We currently wrap silently. (CEL spec says this is optional.)

- [ ] **Custom functions** — User-registered functions beyond the macro system.
      A simpler `Value function(Value[])` registration API for common cases.

- [ ] **Better error messages** — Include source context (the expression
      snippet) in error messages, not just byte offset.

### Lower Priority

- [ ] **`dyn()` type** — Dynamic type assertion, rarely used in practice.

- [ ] **Enum support** — CEL can reference protobuf enum values by name.

- [ ] **Performance** — Currently re-tokenizes on every `evaluate()` call.
      Could cache token arrays or build a simple AST for repeated evaluation.

## 🏗️ Infrastructure

- [ ] **Conformance tests** — Run against the official
      [cel-spec conformance suite](https://github.com/google/cel-spec/tree/master/tests).

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
- Strict bool semantics — `&&`, `||`, and `?:` reject non-bool operands
  with error values rather than coercing.