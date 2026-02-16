/++
 + CEL Value type — a tagged union of all CEL types.
 +/
module decel.value;

import core.time : Duration;
import std.datetime.systime : SysTime;
import std.sumtype;
import std.typecons : Nullable;

/// Lazy value resolution. Subclass to provide deferred/lazy field access.
abstract class Entry
{
    /// Resolve a named field on this entry.
    Value resolve(string name);
}

/// Abstract list — supports indexing and size without materializing.
/// Subclass to provide virtual array access over large datasets.
/// The built-in `ArrayList` wraps a `Value[]` for literal list values.
abstract class List
{
    /// Number of elements in the list.
    abstract size_t length();

    /// Retrieve element at the given index (0-based).
    abstract Value index(size_t i);
}

/// Concrete list backed by a `Value[]` array.
/// Used internally for list literals and list operations.
class ArrayList : List
{
    /// The underlying array storage.
    Value[] elements;

    /// Construct from an existing array.
    this(Value[] elems)
    {
        elements = elems;
    }

    override size_t length()
    {
        return elements.length;
    }

    override Value index(size_t i)
    {
        return elements[i];
    }
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
        duration,
        timestamp,
        err,
    }

    alias Inner = SumType!(typeof(null), // null_
            bool, // bool_
            long, // int_
            ulong, // uint_
            double, // double_
            string, // string_
            immutable(ubyte)[], // bytes_
            List, // list (abstract — ArrayList for literals, subclass for lazy)
            Value[string], // map (string-keyed)
            Entry, // entry (lazy)
            Duration, // duration
            SysTime, // timestamp
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
        return inner.match!((ref typeof(null) _) => Type.null_,
                (ref bool _) => Type.bool_, (ref long _) => Type.int_,
                (ref ulong _) => Type.uint_, (ref double _) => Type.double_,
                (ref string _) => Type.string_, (ref immutable(ubyte)[] _) => Type.bytes_,
                (ref List _) => Type.list, (ref Value[string] _) => Type.map,
                (ref Entry _) => Type.entry, (ref Duration _) => Type.duration,
                (ref SysTime _) => Type.timestamp, (ref Err _) => Type.err,);
    }

    /// Deep equality comparison for Values.
    bool opEquals()(auto ref const Value other) const
    {
        return valueEquals(this, other);
    }

    /// Extract the underlying D value of type T.
    /// Throws if the Value holds an error or a different type.
    T get(T)() const
    {
        auto self = cast(Value) this;
        if (self.type == Type.err)
        {
            auto e = self.inner.match!((ref Err err) => err.message, (ref _) => "unknown error");
            throw new Exception("Value is an error: " ~ e);
        }
        auto result = self.inner.match!((ref T val) => Nullable!T(val), (ref _) => Nullable!T.init);
        if (result.isNull)
        {
            import std.conv : to;

            throw new Exception("Value type mismatch: expected " ~ T.stringof);
        }
        return result.get;
    }

    /// Get the error message from an error Value, or null if not an error.
    string errMessage() const
    {
        auto self = cast(Value) this;
        return self.inner.match!((ref Err e) => e.message, (ref _) => null);
    }

    /// Hash support (needed when opEquals is defined).
    size_t toHash() const nothrow @trusted
    {
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

/// Extract a value of type T from a Value's inner SumType.
/// Asserts that the Value actually holds type T — only call this
/// when the type has already been verified via `Value.type()`.
private T unsafeGet(T)(Value v)
{
    return v.inner.match!((ref T val) => val, (ref _) {
        assert(0, "unsafeGet!" ~ T.stringof ~ ": type tag lied");
        static if (is(T == class) || is(T == interface))
            return null;
        else
            return T.init;
    });
}

/// Deep equality comparison of two Values.
private bool valueEquals(const Value a, const Value b)
{
    auto ma = cast(Value) a;
    auto mb = cast(Value) b;

    auto ta = ma.type();
    auto tb = mb.type();

    if (ta == Value.Type.err || tb == Value.Type.err)
        return false;

    if (ta == Value.Type.null_ && tb == Value.Type.null_)
        return true;

    if (ta == Value.Type.bool_ && tb == Value.Type.bool_)
        return unsafeGet!bool(ma) == unsafeGet!bool(mb);

    if (ta == Value.Type.int_ && tb == Value.Type.int_)
        return unsafeGet!long(ma) == unsafeGet!long(mb);

    if (ta == Value.Type.uint_ && tb == Value.Type.uint_)
        return unsafeGet!ulong(ma) == unsafeGet!ulong(mb);

    if (ta == Value.Type.double_ && tb == Value.Type.double_)
        return unsafeGet!double(ma) == unsafeGet!double(mb);

    if (ta == Value.Type.string_ && tb == Value.Type.string_)
        return unsafeGet!string(ma) == unsafeGet!string(mb);

    if (ta == Value.Type.bytes_ && tb == Value.Type.bytes_)
        return unsafeGet!(immutable(ubyte)[])(ma) == unsafeGet!(immutable(ubyte)[])(mb);

    if (ta == Value.Type.list && tb == Value.Type.list)
    {
        auto la = unsafeGet!List(ma);
        auto lb = unsafeGet!List(mb);
        if (la.length != lb.length)
            return false;
        foreach (i; 0 .. la.length)
            if (!valueEquals(la.index(i), lb.index(i)))
                return false;
        return true;
    }

    if (ta == Value.Type.map && tb == Value.Type.map)
    {
        auto mapA = unsafeGet!(Value[string])(ma);
        auto mapB = unsafeGet!(Value[string])(mb);
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

    if (ta == Value.Type.duration && tb == Value.Type.duration)
        return unsafeGet!Duration(ma) == unsafeGet!Duration(mb);

    if (ta == Value.Type.timestamp && tb == Value.Type.timestamp)
        return unsafeGet!SysTime(ma).toUnixTime() == unsafeGet!SysTime(mb).toUnixTime();

    return false;
}

/// Convenience helper: construct a Value from a D value.
Value value(T)(T val)
{
    return Value(val);
}

/// Convenience: construct a list Value from a Value array.
Value value(T : Value[])(T elems)
{
    return Value(cast(List) new ArrayList(elems));
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

    import core.time : seconds;

    value(30.seconds).type.should.be(Value.Type.duration);

    import std.datetime.systime : SysTime, Clock;

    value(Clock.currTime()).type.should.be(Value.Type.timestamp);

    // List via ArrayList
    value([value(1L), value(2L)]).type.should.be(Value.Type.list);
}

@("Value: deep equality")
unittest
{
    import dshould;

    (value(42L) == value(42L)).should.be(true);
    (value(42L) == value(43L)).should.be(false);
    (value(true) == value(true)).should.be(true);
    (value(true) == value(false)).should.be(false);
    (value("abc") == value("abc")).should.be(true);
    (value("abc") == value("def")).should.be(false);
    (Value.null_() == Value.null_()).should.be(true);

    (value([value(1L), value(2L)]) == value([value(1L), value(2L)])).should.be(true);
    (value([value(1L), value(2L)]) == value([value(1L), value(3L)])).should.be(false);

    (Value.err("a") == Value.err("a")).should.be(false);
}
