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
- [x] Unified List type — abstract `List` class with `ArrayList` concrete impl;
      custom subclasses for lazy/virtual access over large datasets
- [x] `.matches()` regex method — `s.matches(re)` for full-string regex matching
- [x] Null semantics: `null == null` → true, `null != X` → true, null arithmetic → error
- [x] Cross-type numeric comparison (`1u == 1`, `-1 < 1u`, `1u + 1`, etc.)
- [x] `now()` function — returns the current time as a timestamp value
- [x] `Entry.asList()` — entries that double as lists (comprehensions, size, indexing, `in`)
- [x] `Entry.asValue()` — entries that unwrap to scalars in arithmetic/comparison
- [x] `Entry.evalMacro()` — per-entry method macros with full token-stream access

## 🎯 1.0.0 Blockers

- [x] **Duration and timestamp types** — CEL has `google.protobuf.Timestamp`
      and `google.protobuf.Duration` as first-class types. Uses D's
      `core.time.Duration` and `std.datetime.SysTime`.
  - [x] New `Value.Type` variants: `duration` and `timestamp`
  - [x] `duration()` constructor — parse ISO 8601 duration strings (PT1H30M, PT30S, etc.)
  - [x] `timestamp()` constructor — parse RFC 3339 strings
  - [x] Arithmetic: `timestamp - timestamp → duration`,
        `timestamp + duration → timestamp`, `timestamp - duration → timestamp`,
        `duration + duration → duration`, `duration - duration → duration`
  - [x] Comparison: `<`, `<=`, `>`, `>=`, `==`, `!=` for both types
  - [x] Accessor methods: `.hours()`, `.minutes()`, `.seconds()` on duration;
        `.year()`, `.month()`, `.day()`, `.hour()`, `.minute()`, `.second()` on timestamp
  - [x] `string()` cast support for both types

- [x] **Clean public API** — Selective exports in `package.d`, `Value.get!T`
      for type-safe extraction, `Value.errMessage` for error inspection.

- [x] **README with examples** — Usage documentation, API examples,
      quick-start guide.

## 🔮 Post-1.0

### Medium Priority

- [ ] **Performance** — Currently re-tokenizes on every `evaluate()` call.
      Could cache token arrays or build a simple AST for repeated evaluation.

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