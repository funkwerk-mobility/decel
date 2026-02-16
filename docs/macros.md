# Custom Macros

decel supports two kinds of macros for extending the evaluator with
custom syntax: **function-call macros** and **method-call macros**.

## Function-Call Macros

Register custom function-call macros via `evaluateWithMacros()`. A macro
receives the token stream after the opening `(` and must parse its
arguments and consume the closing `)`:

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

All needed types (`Macro`, `TokenRange`, `Token`, `parseExpr`,
`parseArgList`) are exported from the `decel` package.

## Why Macros Instead of Functions?

Macros receive the **raw token stream**, not pre-evaluated arguments.
This gives them full control over:

- **Argument parsing** — decide how many arguments to consume and how
  to parse them
- **Short-circuit evaluation** — skip evaluating some arguments based
  on earlier ones
- **Custom syntax** — parse non-standard syntax like comprehension
  binders (`list.all(x, x > 0)`)

For simpler cases where you just want `f(arg1, arg2)` with pre-evaluated
arguments, the macro can call `parseArgList()` to get a `Value[]`:

```d
customs["sum"] = delegate Value(ref TokenRange r, const Env env, Context ctx) {
    auto args = parseArgList(r, env, ctx);
    r.expect(Token.Kind.rparen);

    long total = 0;
    foreach (arg; args)
        total += arg.get!long;
    return value(total);
};

evaluateWithMacros(`sum(1, 2, 3)`, emptyContext(), customs);  // 6
```

## Method-Call Macros

Method macros are invoked as `expr.name(...)`. They receive the
already-evaluated receiver as their first argument. Register them
via the `Env` struct or use Entry-level macros (see
[entries.md](entries.md#entry-level-method-macros--evalmacro)).

The built-in comprehensions (`.all()`, `.exists()`, `.exists_one()`,
`.map()`, `.filter()`) are implemented as method macros — they need
token-stream access to handle the `(binder, body)` syntax where `body`
is re-parsed for each list element.

## Macro API Reference

### Types

```d
/// Function-call macro: called when the parser sees `name(...)`.
/// The opening '(' has already been consumed.
alias Macro = Value delegate(ref TokenRange r, const Env env, Context ctx);

/// Method-call macro: called when the parser sees `expr.name(...)`.
/// The opening '(' has already been consumed.
/// `target` is the already-evaluated receiver expression.
alias MethodMacro = Value delegate(Value target, ref TokenRange r, const Env env, Context ctx);
```

### TokenRange

The token cursor provides these methods:

```d
struct TokenRange
{
    Token peek();              // current token (without consuming)
    Token advance();           // consume and return current token
    Token expect(Token.Kind);  // consume expected kind, or throw EvalException
    bool match(Token.Kind);    // consume if matching, return true/false
    TokenRange save();         // snapshot for replay
}
```

### Helpers

```d
/// Parse a comma-separated argument list (stops at ')' without consuming it).
Value[] parseArgList(ref TokenRange r, const Env env, Context ctx);

/// Parse an expression with the given minimum precedence.
Value parseExpr(ref TokenRange r, const Env env, Context ctx, int minPrec);
```