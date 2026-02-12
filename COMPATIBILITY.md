# decel — CEL Compatibility Notes

This document tracks intentional deviations from the
[CEL specification](https://github.com/google/cel-spec).

## String-only map keys

The CEL spec allows `bool`, `int`, `uint`, and `string` as map keys.
decel only supports `string` keys (`Value[string]`).

**Rationale**: Non-string map keys add complexity with minimal practical
benefit. Every real-world CEL usage we've seen uses string keys. This
keeps the implementation simple and idiomatic in D.