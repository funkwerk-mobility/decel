/++
 + CEL evaluation context — provides variable bindings.
 +/
module decel.context;

import decel.value;

/// A Context resolves top-level identifiers to Values.
/// It is simply a callable that maps names to values.
alias Context = Value delegate(string name);

/// Create a context from an associative array (useful for tests).
Context contextFrom(Value[string] bindings)
{
    return (string name) {
        if (auto p = name in bindings)
            return *p;
        return Value.err("undefined");
    };
}

/// Empty context that resolves nothing.
Context emptyContext()
{
    return (string) => Value.err("undefined");
}

/// Layer a single binding on top of an existing context.
Context bindContext(Context outer, string name, Value val)
{
    return (string n) {
        if (n == name)
            return val;
        return outer(n);
    };
}

@("Context: resolve bindings")
unittest
{
    import dshould;

    auto ctx = contextFrom(["x": value(10L), "name": value("alice"),]);

    ctx("x").type.should.be(Value.Type.int_);
    ctx("name").type.should.be(Value.Type.string_);
    ctx("missing").type.should.be(Value.Type.err);
}
