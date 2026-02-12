/++
 + CEL Value type — a tagged union of all CEL types.
 +/
module decel.value;

import std.sumtype;

/// Lazy value resolution. Subclass to provide deferred/lazy field access.
abstract class Entry
{
    /// Resolve a named field on this entry.
    Value resolve(string name);
}

/// A CEL value: the runtime representation of any CEL expression result.
struct Value
{
    /// The set of types a CEL Value can hold.
    enum Type
    {
        null_,
        bool_,
        int_,
        uint_,
        double_,
        string_,
        bytes_,
        list,
        map,
        entry,
        err,
    }

    alias Inner = SumType!(typeof(null), // null_
            bool, // bool_
            long, // int_
            ulong, // uint_
            double, // double_
            string, // string_
            immutable(ubyte)[], // bytes_
            Value[], // list
            Value[string], // map (string-keyed)
            Entry, // entry (lazy)
            Err, // err
            );

    /// The underlying SumType storage.
    Inner inner;
    alias inner this;

    /// Error sentinel — carries a message describing what went wrong.
    static struct Err
    {
        /// The error message.
        string message;
    }

    /// Construct a Value from any supported type.
    this(T)(T val)
    {
        inner = Inner(val);
    }

    /// Convenience: null value.
    static Value null_()
    {
        Value v;
        v.inner = Inner(null);
        return v;
    }

    /// Convenience: error value.
    static Value err(string msg)
    {
        Value v;
        v.inner = Inner(Err(msg));
        return v;
    }

    /// Returns the type tag for this value.
    Type type()
    {
        // Use explicit ref handlers to avoid SumType matching ambiguities.
        return inner.match!((ref typeof(null) _) => Type.null_,
                (ref bool _) => Type.bool_, (ref long _) => Type.int_,
                (ref ulong _) => Type.uint_, (ref double _) => Type.double_,
                (ref string _) => Type.string_, (ref immutable(ubyte)[] _) => Type.bytes_,
                (ref Value[] _) => Type.list, (ref Value[string] _) => Type.map,
                (ref Entry _) => Type.entry, (ref Err _) => Type.err,);
    }

    /// Deep equality comparison for Values.
    bool opEquals()(auto ref const Value other) const
    {
        return valueEquals(this, other);
    }

    /// Hash support (needed when opEquals is defined).
    size_t toHash() const nothrow @trusted
    {
        // Simple hash — just use type tag. Not ideal for hash tables,
        // but correct and sufficient for now.
        try
        {
            auto self = cast(Value) this;
            return hashOf(cast(int) self.type());
        }
        catch (Exception)
        {
            return 0;
        }
    }
}

/// Deep equality comparison of two Values.
private bool valueEquals(const Value a, const Value b)
{
    // We cast away const to use SumType.match (which requires mutable ref).
    auto ma = cast(Value) a;
    auto mb = cast(Value) b;

    auto ta = ma.type();
    auto tb = mb.type();

    // Different types are never equal (except cross-numeric, handled below)
    if (ta == Value.Type.err || tb == Value.Type.err)
        return false;

    if (ta == Value.Type.null_ && tb == Value.Type.null_)
        return true;

    if (ta == Value.Type.bool_ && tb == Value.Type.bool_)
    {
        auto va = ma.inner.match!((ref bool v) => v, (ref _) => false);
        auto vb = mb.inner.match!((ref bool v) => v, (ref _) => false);
        return va == vb;
    }

    if (ta == Value.Type.int_ && tb == Value.Type.int_)
    {
        auto va = ma.inner.match!((ref long v) => v, (ref _) => long.min);
        auto vb = mb.inner.match!((ref long v) => v, (ref _) => long.min);
        return va == vb;
    }

    if (ta == Value.Type.uint_ && tb == Value.Type.uint_)
    {
        auto va = ma.inner.match!((ref ulong v) => v, (ref _) => ulong.max);
        auto vb = mb.inner.match!((ref ulong v) => v, (ref _) => ulong.max);
        return va == vb;
    }

    if (ta == Value.Type.double_ && tb == Value.Type.double_)
    {
        auto va = ma.inner.match!((ref double v) => v, (ref _) => double.nan);
        auto vb = mb.inner.match!((ref double v) => v, (ref _) => double.nan);
        return va == vb;
    }

    if (ta == Value.Type.string_ && tb == Value.Type.string_)
    {
        auto va = ma.inner.match!((ref string v) => v, (ref _) => null);
        auto vb = mb.inner.match!((ref string v) => v, (ref _) => null);
        return va == vb;
    }

    if (ta == Value.Type.bytes_ && tb == Value.Type.bytes_)
    {
        auto va = ma.inner.match!((ref immutable(ubyte)[] v) => v, (ref _) => null);
        auto vb = mb.inner.match!((ref immutable(ubyte)[] v) => v, (ref _) => null);
        return va == vb;
    }

    if (ta == Value.Type.list && tb == Value.Type.list)
    {
        auto la = ma.inner.match!((ref Value[] v) => v, (ref _) => null);
        auto lb = mb.inner.match!((ref Value[] v) => v, (ref _) => null);
        if (la.length != lb.length)
            return false;
        foreach (i; 0 .. la.length)
            if (!valueEquals(la[i], lb[i]))
                return false;
        return true;
    }

    if (ta == Value.Type.map && tb == Value.Type.map)
    {
        auto mapA = ma.inner.match!((ref Value[string] v) => v, (ref _) => null);
        auto mapB = mb.inner.match!((ref Value[string] v) => v, (ref _) => null);
        if (mapA.length != mapB.length)
            return false;
        foreach (k, v; mapA)
        {
            auto pb = k in mapB;
            if (pb is null || !valueEquals(v, *pb))
                return false;
        }
        return true;
    }

    return false;
}

/// Convenience helper: construct a Value from a D value.
Value value(T)(T val)
{
    return Value(val);
}

@("Value: construct and check types")
unittest
{
    import dshould;

    value(true).type.should.be(Value.Type.bool_);
    value(42L).type.should.be(Value.Type.int_);
    value(42UL).type.should.be(Value.Type.uint_);
    value(3.14).type.should.be(Value.Type.double_);
    value("hello").type.should.be(Value.Type.string_);
    Value.null_().type.should.be(Value.Type.null_);
    Value.err("oops").type.should.be(Value.Type.err);
}

@("Value: deep equality")
unittest
{
    import dshould;

    // Primitives
    (value(42L) == value(42L)).should.be(true);
    (value(42L) == value(43L)).should.be(false);
    (value(true) == value(true)).should.be(true);
    (value(true) == value(false)).should.be(false);
    (value("abc") == value("abc")).should.be(true);
    (value("abc") == value("def")).should.be(false);
    (Value.null_() == Value.null_()).should.be(true);

    // Lists
    (value([value(1L), value(2L)]) == value([value(1L), value(2L)])).should.be(true);
    (value([value(1L), value(2L)]) == value([value(1L), value(3L)])).should.be(false);

    // Error values are never equal
    (Value.err("a") == Value.err("a")).should.be(false);
}
