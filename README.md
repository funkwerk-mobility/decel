# decel

A [Common Expression Language (CEL)](https://cel.dev/) evaluator for D.
CEL is a non-Turing-complete expression language designed for evaluating
simple, safe expressions — typically policy rules, filters, or validation
checks.

decel implements the [CEL spec](https://github.com/google/cel-spec) as a
single-pass interpreter with no AST. See [COMPATIBILITY.md](COMPATIBILITY.md)
for known deviations.

## Installation

Add to your `dub.sdl`:

```sdl
dependency "decel" version="~>1.0"
```

Or `dub.json`:

```json
"dependencies": { "decel": "~>1.0" }
```

## Usage

```d
import decel;

// Evaluate an expression
auto result = evaluate(`1 + 2`, emptyContext());
assert(result == value(3L));

// With variables
auto ctx = contextFrom([
    "user":  value("alice"),
    "role":  value("admin"),
    "level": value(5L),
]);
auto allowed = evaluate(`role == "admin" || level >= 10`, ctx);
assert(allowed == value(true));

// Extract D types from results
auto n = evaluate(`2 + 3`, emptyContext()).get!long;    // 5
auto s = evaluate(`"hi"`, emptyContext()).get!string;    // "hi"

// Check for errors
auto err = evaluate(`1 / 0`, emptyContext());
assert(err.type == Value.Type.err);
assert(err.errMessage == "division by zero");
```

## Types

CEL type          | D storage type              | Literal examples
----------------- | --------------------------- | ----------------
`int`             | `long`                      | `42`, `-1`, `0xFF`
`uint`            | `ulong`                     | `42u`, `0xFFu`
`double`          | `double`                    | `3.14`, `1.0`
`bool`            | `bool`                      | `true`, `false`
`string`          | `string`                    | `"hello"`, `'world'`, `"""multi"""`
`bytes`           | `immutable(ubyte)[]`        | `b"abc"`
`null_type`       | `typeof(null)`              | `null`
`list`            | `Value[]`                   | `[1, 2, 3]`
`map`             | `Value[string]`             | `{"key": "value"}`
`duration`        | `core.time.Duration`        | `duration("PT1H30M")`
`timestamp`       | `std.datetime.SysTime`      | `timestamp("2023-01-15T12:00:00Z")`

Cross-type numeric operations work naturally: `1u == 1` is `true`,
`1 + 1.5` promotes to `double`.

## Operators

```
Arithmetic:   +  -  *  /  %
Comparison:   ==  !=  <  <=  >  >=
Logical:      &&  ||  !
Conditional:  ? :
Membership:   in
Index:        []
Member:       .
```

`&&` and `||` short-circuit: `false && 1/0` evaluates to `false`.
Logical operators enforce strict bool semantics — non-bool operands
produce an error value, not implicit coercion.

## Functions and Methods

```cel
// Size
size([1, 2, 3])        // 3
"hello".size()         // 5

// String methods
"hello world".contains("world")     // true
"hello".startsWith("hel")           // true
"hello".endsWith("llo")             // true
"abc123".matches("[a-z]+[0-9]+")    // true (full-string match)

// Type inspection
type(42)               // "int"
type("hello")          // "string"

// Type casts
int("42")              // 42
double(42)             // 42.0
string(42)             // "42"
uint(1)                // 1u

// Existence check
has(request.auth)      // true if auth field exists (not an error)

// Membership
"x" in {"x": 1}       // true
2 in [1, 2, 3]         // true
"el" in "hello"        // true
```

## Comprehensions

List comprehensions use a `list.method(var, expr)` syntax:

```cel
[1, 2, 3, 4, 5].filter(x, x > 3)         // [4, 5]
[1, 2, 3].map(x, x * 2)                   // [2, 4, 6]
[1, 2, 3].all(x, x > 0)                   // true
[1, 2, 3].exists(x, x == 2)               // true
[1, 2, 3].exists_one(x, x > 2)            // true
```

Comprehensions chain: `[1,2,3,4].map(x, x*2).filter(y, y > 4)` → `[6, 8]`.

## Duration and Timestamp

Durations use [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Durations)
format. Timestamps use [RFC 3339](https://www.rfc-editor.org/rfc/rfc3339).

```cel
duration("PT1H30M").minutes()     // 90
duration("PT1H") + duration("PT30M") == duration("PT1H30M")  // true

timestamp("2023-01-15T12:00:00Z").year()   // 2023
timestamp("2023-01-15T12:00:00Z") + duration("PT1H")
    == timestamp("2023-01-15T13:00:00Z")   // true
```

You can also pass D values directly via the context:

```d
import core.time : seconds, hours;
import std.datetime.systime : SysTime, Clock;

auto ctx = contextFrom([
    "timeout": value(30.seconds),
    "created": value(Clock.currTime()),
]);
evaluate("timeout.seconds() > 10", ctx);  // true
```

## Nested Objects via Entry

For structured data, subclass `Entry` to provide lazy field resolution:

```d
class Request : Entry
{
    override Value resolve(string name)
    {
        switch (name)
        {
            case "method": return value("GET");
            case "path":   return value("/api/users");
            case "auth":   return value(["user": value("alice")]);
            default:       return Value.err("no such field: " ~ name);
        }
    }
}

auto ctx = contextFrom(["request": Value(new Request())]);
evaluate(`request.method == "GET"`, ctx);      // true
evaluate(`request.auth.user`, ctx);            // "alice"
evaluate(`has(request.auth)`, ctx);            // true
evaluate(`has(request.missing)`, ctx);         // false
```

## Lazy Lists via EntryList

For large datasets, subclass `EntryList` to provide lazy indexing and size
without materializing an array:

```d
class DatabaseRows : EntryList
{
    override size_t length() { return 1_000_000; }

    override Value index(size_t i)
    {
        // Fetch row i on demand
        return value(cast(long) i);
    }
}

auto ctx = contextFrom(["rows": Value(new DatabaseRows())]);
evaluate("rows[42]", ctx);            // fetches only row 42
evaluate("size(rows)", ctx);           // 1000000 (no materialization)
evaluate("rows.exists(r, r == 42)", ctx);  // iterates lazily
```

`EntryList` supports `size()`, `[index]`, `in`, and all comprehensions
(`.all()`, `.exists()`, `.filter()`, `.map()`, etc.).

## Error Handling

Parse errors (syntax errors, unexpected tokens) throw `EvalException`:

```d
try
    evaluate(`1 +`, emptyContext());
catch (EvalException e)
    writeln(e.msg);  // "at position 3: unexpected eof"
```

Evaluation errors (type mismatches, division by zero, missing keys) are
returned as error *values*, not exceptions:

```d
auto result = evaluate(`1 / 0`, emptyContext());
if (result.type == Value.Type.err)
    writeln(result.errMessage);  // "division by zero"
```

Errors propagate through operators and are absorbed by short-circuit
logic: `false && (1/0 == 1)` → `false`.

## Custom Macros

Extend the evaluator with custom function-call macros:

```d
import decel;
import decel.env : TokenRange, Env;
import decel.eval : parseExpr;

Macro[string] customs;
customs["always_true"] = delegate Value(ref TokenRange r, const Env env, Context ctx) {
    parseExpr(r, env, ctx, 0);  // parse and discard the argument
    r.expect(Token.Kind.rparen); // consume closing ')'
    return value(true);
};

evaluateWithMacros(`always_true(anything)`, emptyContext(), customs);  // true
```

Macros receive the token stream and are responsible for parsing their own
arguments and consuming the closing `)`.

## License

BSL-1.0