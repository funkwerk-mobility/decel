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

## Regex flavor

CEL specifies RE2 syntax for `.matches()`. decel uses D's `std.regex`,
which is PCRE-flavored. In practice the overlap covers the vast majority
of patterns. Exotic RE2 features (e.g. named Unicode classes) may differ.

## Unimplemented features

- Bytes operations — tokenized but not fully operational
- Integer overflow detection — wraps silently (CEL spec says this is optional)
- `dyn()` type, enum support