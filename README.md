# decel

A [Common Expression Language (CEL)](https://cel.dev/) evaluator for D.
CEL is a non-Turing-complete expression language designed for evaluating
simple, safe expressions — typically policy rules, filters, or validation
checks.

decel implements the [CEL spec](https://github.com/google/cel-spec) as a
single-pass interpreter with no AST. See [COMPATIBILITY.md](COMPATIBILITY.md)
for known deviations.

## AI Notice

This repo is ~entirely developed by Claude 4.6 Opus. Good job Opus!

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
// .get!T throws EvalException if the Value holds a different type or an error

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
`list`            | `List` (`ArrayList`)        | `[1, 2, 3]`
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

// Current time
now()                  // current timestamp (UTC)

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

## Integrating Your Data

decel provides two abstract classes for exposing D data structures to
CEL expressions without converting everything to `Value` up front.

### Entry — Lazy Field Access

Subclass `Entry` to expose an object with named fields. The `.field`
and `["field"]` syntax both call `resolve()`. Return `Value.err` for
unknown fields — this makes `has()` work automatically.

```d
class HttpRequest : Entry
{
    private string _method;
    private string _path;
    private string[string] _headers;

    this(string method, string path, string[string] headers)
    {
        _method = method;
        _path = path;
        _headers = headers;
    }

    override Value resolve(string name)
    {
        switch (name)
        {
            case "method":  return value(_method);
            case "path":    return value(_path);
            case "headers":
                Value[string] hmap;
                foreach (k, v; _headers)
                    hmap[k] = value(v);
                return Value(hmap);
            default:
                return Value.err("no such field: " ~ name);
        }
    }
}

auto req = new HttpRequest("GET", "/api/users",
    ["Content-Type": "application/json", "Authorization": "Bearer tok"]);
auto ctx = contextFrom(["request": Value(cast(Entry) req)]);

evaluate(`request.method == "GET"`, ctx);                // true
evaluate(`request.path.startsWith("/api")`, ctx);        // true
evaluate(`"Authorization" in request.headers`, ctx);     // true
evaluate(`has(request.method)`, ctx);                    // true
evaluate(`has(request.missing)`, ctx);                   // false
```

### List — Lazy Indexing

Subclass `List` to expose a sequence with `length()` and `index(i)`
without materializing a `Value[]` array. All list operations work:
`size()`, `[i]`, `in`, `+`, and comprehensions.

```d
class DatabaseRows : List
{
    private long _count;

    this(long count) { _count = count; }

    override size_t length() { return cast(size_t) _count; }

    override Value index(size_t i)
    {
        // Fetch row i on demand from your database
        return value(cast(long) i);
    }
}

auto ctx = contextFrom(["rows": Value(cast(List) new DatabaseRows(1_000_000))]);
evaluate("rows[42]", ctx);                     // fetches only row 42
evaluate("size(rows)", ctx);                   // 1000000 (no materialization)
evaluate("rows.exists(r, r == 42)", ctx);      // iterates lazily
```

### Entry as List — `asList()`

Override `asList()` to make an Entry usable in list contexts (comprehensions,
`size()`, `in`, indexing) while still supporting named field access:

```d
class MetricSeries : Entry
{
    private string _name;
    private Value[] _dataPoints;

    this(string name, Value[] data)
    {
        _name = name;
        _dataPoints = data;
    }

    override Value resolve(string name)
    {
        switch (name)
        {
            case "name": return value(_name);
            default:     return Value.err("no such field: " ~ name);
        }
    }

    override List asList()
    {
        return new ArrayList(_dataPoints);
    }
}

auto series = new MetricSeries("cpu_usage", [value(10L), value(20L), value(30L)]);
auto ctx = contextFrom(["metric": Value(cast(Entry) series)]);

// Field access still works
evaluate(`metric.name`, ctx);                    // "cpu_usage"

// List operations work via asList()
evaluate(`size(metric)`, ctx);                   // 3
evaluate(`metric[0]`, ctx);                      // 10
evaluate(`20 in metric`, ctx);                   // true
evaluate(`metric.filter(x, x > 15)`, ctx);       // [20, 30]
evaluate(`metric.all(x, x > 0)`, ctx);           // true
```

### Entry as Scalar — `asValue()`

Override `asValue()` to let an Entry unwrap to a scalar value in
arithmetic and comparison contexts:

```d
class Gauge : Entry
{
    private double _value;
    private string _unit;

    this(double v, string unit)
    {
        _value = v;
        _unit = unit;
    }

    override Value resolve(string name)
    {
        switch (name)
        {
            case "unit": return value(_unit);
            default:     return Value.err("no such field: " ~ name);
        }
    }

    override Nullable!Value asValue()
    {
        import std.typecons : nullable;
        return nullable(value(_value));
    }
}

auto gauge = new Gauge(42.5, "percent");
auto ctx = contextFrom(["cpu": Value(cast(Entry) gauge)]);

evaluate(`cpu.unit`, ctx);          // "percent"
evaluate(`cpu > 40.0`, ctx);        // true
evaluate(`cpu + 7.5`, ctx);         // 50.0
evaluate(`cpu == 42.5`, ctx);       // true
```

Both `asList()` and `asValue()` can be overridden on the same Entry,
giving you a value that has named fields, acts as a list, and participates
in arithmetic.

### Entry-Level Method Macros — `methodMacro()`

Override `methodMacro()` to give an Entry its own method-call handlers
with full access to the token stream. This lets each Entry subclass define
custom syntax without registering global macros:

```d
class MetricEntry : Entry
{
    string name;
    double[] values;

    override Value resolve(string field) { /* ... */ }
    override List asList() { /* ... */ }

    override MethodMacro methodMacro(string name)
    {
        if (name == "where")
        {
            // Return a handler that parses .where(...) arguments
            return (Value target, ref TokenRange r, const Env env, Context ctx) {
                auto args = parseArgList(r, env, ctx);
                r.expect(Token.Kind.rparen);
                // ... filter logic, return new MetricEntry ...
            };
        }
        return null;  // fall through to normal method dispatch
    }
}
```

Then in CEL:
```cel
metric.where(threshold > 50).all(x, x.healthy)
```

The evaluator checks Entry-level macros after global method macros but
before built-in methods. Return `null` to fall through to normal dispatch.

### Combining Entry and List

For real-world data models, nest Entry and List to expose a complete
object graph. CEL expressions navigate it naturally:

```d
class User : Entry
{
    string name;
    string role;

    override Value resolve(string field)
    {
        switch (field)
        {
            case "name": return value(name);
            case "role": return value(role);
            default:     return Value.err("no such field: " ~ field);
        }
    }
}

class UserList : List
{
    User[] users;

    override size_t length() { return users.length; }

    override Value index(size_t i)
    {
        return Value(cast(Entry) users[i]);
    }
}

auto users = new UserList();
users.users = [makeUser("alice", "admin"), makeUser("bob", "viewer")];

auto ctx = contextFrom([
    "users": Value(cast(List) users),
    "minRole": value("admin"),
]);

// Navigate the full object graph from CEL
evaluate(`users[0].name`, ctx);                              // "alice"
evaluate(`users.exists(u, u.role == "admin")`, ctx);         // true
evaluate(`users.filter(u, u.role == minRole).size()`, ctx);  // 1
evaluate(`users.all(u, has(u.name))`, ctx);                  // true
```

### Scalar Values

For simple bindings, use `value()` to wrap D types directly:

```d
auto ctx = contextFrom([
    "name":      value("alice"),
    "level":     value(5L),
    "active":    value(true),
    "score":     value(3.14),
    "tags":      value([value("a"), value("b")]),
    "metadata":  value(["env": value("prod"), "region": value("us-east-1")]),
]);
```

`value()` accepts `long`, `ulong`, `double`, `bool`, `string`,
`Value[]` (creates an `ArrayList`), `Duration`, and `SysTime`.

> **Note on casts**: When passing `Entry` or `List` subclasses into a
> `Value`, use an explicit cast: `Value(cast(Entry) myEntry)` or
> `Value(cast(List) myList)`. This is needed because D's `SumType`
> stores the abstract base class, not your concrete subclass.

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

Macro[string] customs;
customs["always_true"] = delegate Value(ref TokenRange r, const Env env, Context ctx) {
    parseExpr(r, env, ctx, 0);  // parse and discard the argument
    r.expect(Token.Kind.rparen); // consume closing ')'
    return value(true);
};

evaluateWithMacros(`always_true(anything)`, emptyContext(), customs);  // true
```

Macros receive the token stream and are responsible for parsing their own
arguments and consuming the closing `)`. All needed types (`Macro`,
`TokenRange`, `Token`, `parseExpr`) are exported from the `decel` package.

## License

BSL-1.0
