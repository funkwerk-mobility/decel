# decel — CEL Compatibility Notes

This document tracks intentional deviations from the
[CEL specification](https://github.com/google/cel-spec).

## String-only map keys

The CEL spec allows `bool`, `int`, `uint`, and `string` as map keys.
decel only supports `string` keys (`Value[string]`).

**Rationale**: Non-string map keys add complexity with minimal practical
benefit. Every real-world CEL usage we've seen uses string keys. This
keeps the implementation simple and idiomatic in D.

## No compile/check step

The CEL spec envisions a compile → check → evaluate pipeline.
decel evaluates in a single pass: tokenize → interpret.
There is no AST, no type-checking phase, and no program caching.

**Rationale**: decel targets low-frequency query evaluation (web API
filtering). Queries are not reused, so caching provides no benefit.
Single-pass keeps the implementation simple and latency low.

## Unimplemented features

- `has()` macro — requires unevaluated argument handling
- `.matches()` — regex support not yet implemented
- Bytes literals — tokenized but not fully tested
- Overload resolution — no custom function dispatch yet