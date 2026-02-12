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

/// A key-value pair for CEL map values.
/// CEL allows bool, int, uint, and string as map keys.
struct MapEntry
{
    /// The map entry's key (must be bool, int, uint, or string).
    Value key;
    /// The map entry's value.
    Value val;
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
            MapEntry[], // map (ordered key-value pairs)
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
                (ref Value[] _) => Type.list, (ref MapEntry[] _) => Type.map,
                (ref Entry _) => Type.entry, (ref Err _) => Type.err,);
    }
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
