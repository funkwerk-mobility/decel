# Entry and List — Integrating Your Data

decel provides two abstract classes for exposing D data structures to
CEL expressions without converting everything to `Value` up front.

## Entry — Lazy Field Access

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

## List — Lazy Indexing

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

## Entry as List — `asList()`

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

## Entry as Scalar — `asValue()`

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

## Entry-Level Method Macros — `evalMacro()`

Override `evalMacro()` to give an Entry its own method-call handlers
with full access to the token stream, environment, and context. This lets
each Entry subclass define custom syntax without registering global macros:

```d
class MetricEntry : Entry
{
    override Value resolve(string field) { /* ... */ }
    override List asList() { /* ... */ }

    override Nullable!Value evalMacro(string name, Value self,
            ref TokenRange r, const Env env, Context ctx)
    {
        if (name == "where")
        {
            auto args = parseArgList(r, env, ctx);
            r.expect(Token.Kind.rparen);
            // ... filter logic, return nullable(Value(...)) ...
        }
        return Nullable!Value.init;  // fall through to normal dispatch
    }
}
```

Then in CEL:
```cel
metric.where(threshold > 50).all(x, x.healthy)
```

The evaluator checks Entry-level macros after global method macros but
before built-in methods. Return `Nullable!Value.init` to fall through.

## Entry Continuation Parsing — `evalContinuation()`

Override `evalContinuation()` to parse arbitrary syntax immediately after
an expression resolves to your Entry — before the evaluator checks for
`.`, `[`, or any binary operator. This is the hook for custom postfix
syntax like Prometheus-style `{attribute == "value"}` filters:

```d
class MetricEntry : Entry
{
    // ... resolve(), asList(), etc.

    override Nullable!Value evalContinuation(Value self,
            ref TokenRange r, const Env env, Context ctx)
    {
        if (r.peek().kind != Token.Kind.lbrace)
            return Nullable!Value.init;  // not our syntax, fall through

        r.advance(); // consume '{'

        // Parse key == "value" filter pairs
        string[string] filters;
        if (r.peek().kind != Token.Kind.rbrace)
        {
            auto key = r.expect(Token.Kind.ident);
            r.expect(Token.Kind.eqEq);
            auto val = parseExpr(r, env, ctx, 0);
            filters[key.text] = val.get!string;

            while (r.match(Token.Kind.comma))
            {
                key = r.expect(Token.Kind.ident);
                r.expect(Token.Kind.eqEq);
                val = parseExpr(r, env, ctx, 0);
                filters[key.text] = val.get!string;
            }
        }
        r.expect(Token.Kind.rbrace);

        // Return filtered metric
        return nullable(Value(cast(Entry) filterSeries(filters)));
    }
}
```

Then in CEL — exactly Prometheus syntax:
```cel
http_requests_total{method = "GET", status = "200"}[0].value
http_requests_total{method = "GET"}.all(s, s.value > 100)
http_requests_total{}.count
```

Both `=` and `==` are accepted inside attribute filters.

The continuation fires at the highest precedence (same level as `.` and
`[]`), so `metric{...}.field` and `metric{...}[0]` chain naturally.
Return `Nullable!Value.init` to fall through to normal parsing. The
result can itself be an Entry, so continuations can chain.

## Combining Entry and List

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

## Scalar Values

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