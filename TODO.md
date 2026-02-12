# decel — TODO

## Open Questions

- **`has()` semantics**: CEL's `has(x.y)` is a macro — the argument `x.y`
  should not be evaluated, just tested for existence. This needs special
  handling in the parser (don't evaluate the argument, just check if the
  field/key exists). Current implementation throws "not yet implemented".

- **Deep equality for `in` on lists**: The `in` operator on lists needs
  to compare Values for equality, which requires implementing `opEquals`
  on `Value`. Currently always returns `false` for list membership.

- **Null handling**: CEL has specific null propagation rules (e.g.,
  `null == null` is `true`, `null + 1` is an error). Need to verify
  our behavior matches.

## Not Yet Implemented

- [ ] `has()` macro
- [ ] `.matches()` regex method
- [ ] Bytes literal evaluation (tokenized, not interpreted)
- [ ] Custom function registration
- [ ] `Value.opEquals` for deep equality
- [ ] Negative list indexing (e.g., `list[-1]`) — CEL doesn't support
      this but we currently do. Decide if this is a feature or a bug.

## Architecture Notes

- Single-pass tokenize → evaluate. No AST.
- Error values (`Value.err`) for evaluation errors (division by zero,
  type mismatches, missing keys). Errors propagate through operators
  and are naturally absorbed by short-circuit `&&` and `||`.
- `EvalException` only for parse/syntax errors (unexpected token, etc.).
- Pratt parsing for operator precedence.
- Short-circuit `&&` and `||` — both sides are always parsed, but
  `false && err` → `false` and `true || err` → `true`.