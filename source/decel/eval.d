/++
 + CEL Evaluator — single-pass recursive descent interpreter.
 +
 + Tokenizes and evaluates a CEL expression in one pass, producing a Value.
 + Uses Pratt parsing for correct operator precedence.
 +
 + Three concerns are cleanly separated:
 + $(UL
 +   $(LI `TokenRange` — linear parser state (token cursor))
 +   $(LI `Env` — global configuration (macros, method macros))
 +   $(LI `Context` — lexical scope (variable bindings))
 + )
 +
 + Every parse/eval function takes all three explicitly, making data flow
 + clear and avoiding mutation of shared state.
 +/
module decel.eval;

import decel.context;
import decel.env;
import decel.lexer;
import decel.value;

import core.time : Duration, hours, minutes, seconds, hnsecs, dur;
import std.conv : to, ConvException;
import std.datetime.systime : SysTime;
import std.datetime.timezone : UTC;
import std.sumtype : match;
import std.typecons : Nullable, nullable;

// Re-export EvalException and kindName so existing callers still work.
public import decel.env : EvalException, kindName;

/// Evaluate a CEL expression string against a context.
Value evaluate(string source, Context ctx)
{
    auto env = standardEnv();
    auto tokens = tokenize(source);
    auto r = TokenRange(tokens, 0);
    auto result = parseExpr(r, env, ctx, 0);
    r.expect(Token.Kind.eof);
    return result;
}

/// Evaluate a CEL expression with custom macros.
Value evaluateWithMacros(string source, Context ctx, Macro[string] macros)
{
    auto env = standardEnv();
    if (macros !is null)
    {
        foreach (k, v; macros)
            env.macros[k] = v;
    }
    auto tokens = tokenize(source);
    auto r = TokenRange(tokens, 0);
    auto result = parseExpr(r, env, ctx, 0);
    r.expect(Token.Kind.eof);
    return result;
}

/// Build the standard environment with all built-in macros.
private Env standardEnv()
{
    Env e;
    e.macros = builtinMacros();
    e.methodMacros = builtinMethodMacros();
    return e;
}

// ── Expression parsing (Pratt) ──────────────────────────────────────

/// Parse an expression with the given minimum precedence.
Value parseExpr(ref TokenRange r, const Env env, Context ctx, int minPrec)
{
    auto lhs = parseUnary(r, env, ctx);

    while (true)
    {
        auto tok = r.peek();
        auto prec = infixPrec(tok.kind);
        if (prec < 0 || prec < minPrec)
            break;

        // Ternary conditional: expr ? expr : expr
        if (tok.kind == Token.Kind.question)
        {
            r.advance();
            lhs = parseTernary(r, env, ctx, lhs);
            continue;
        }

        // `in` operator
        if (tok.kind == Token.Kind.inKw)
        {
            r.advance();
            auto rhs = parseExpr(r, env, ctx, prec + 1);
            lhs = evalIn(lhs, rhs);
            continue;
        }

        // Member access: expr.ident
        if (tok.kind == Token.Kind.dot)
        {
            r.advance();
            auto ident = r.expect(Token.Kind.ident);

            // Method call: expr.ident(args...)
            if (r.peek().kind == Token.Kind.lparen)
            {
                r.advance();
                // Check method macros first
                if (auto mm = ident.text in env.methodMacros)
                {
                    lhs = (*mm)(lhs, r, env, ctx);
                    continue;
                }
                auto args = parseArgList(r, env, ctx);
                r.expect(Token.Kind.rparen);
                lhs = evalMethod(lhs, ident.text, args);
            }
            else
            {
                lhs = evalMember(lhs, ident.text);
            }
            continue;
        }

        // Index: expr[expr]
        if (tok.kind == Token.Kind.lbracket)
        {
            r.advance();
            auto index = parseExpr(r, env, ctx, 0);
            r.expect(Token.Kind.rbracket);
            lhs = evalIndex(lhs, index);
            continue;
        }

        // Binary operator
        r.advance();

        // Short-circuit: && and ||
        if (tok.kind == Token.Kind.ampAmp)
        {
            auto rhs = parseExpr(r, env, ctx, prec + 1);
            if (isFalsy(lhs))
                continue; // lhs stays false, absorbs RHS errors
            if (isTruthy(lhs))
            {
                lhs = toBool(rhs);
                continue;
            }
            // lhs is non-bool error — propagate
            lhs = Value.err("'&&' operand is not a bool: " ~ typeName(lhs));
            continue;
        }
        if (tok.kind == Token.Kind.pipePipe)
        {
            auto rhs = parseExpr(r, env, ctx, prec + 1);
            if (isTruthy(lhs))
                continue; // lhs stays true, absorbs RHS errors
            if (isFalsy(lhs))
            {
                lhs = toBool(rhs);
                continue;
            }
            // lhs is non-bool error — propagate
            lhs = Value.err("'||' operand is not a bool: " ~ typeName(lhs));
            continue;
        }

        auto rhs = parseExpr(r, env, ctx, prec + 1);
        lhs = evalBinary(tok.kind, lhs, rhs);
    }

    return lhs;
}

/// Parse a unary expression (prefix operators and atoms).
private Value parseUnary(ref TokenRange r, const Env env, Context ctx)
{
    auto tok = r.peek();

    if (tok.kind == Token.Kind.minus)
    {
        r.advance();
        auto operand = parseUnary(r, env, ctx);
        return evalUnaryMinus(operand);
    }

    if (tok.kind == Token.Kind.bang)
    {
        r.advance();
        auto operand = parseUnary(r, env, ctx);
        return evalUnaryNot(operand);
    }

    return parseAtom(r, env, ctx);
}

/// Parse an atomic expression (literals, identifiers, parens, lists, maps).
private Value parseAtom(ref TokenRange r, const Env env, Context ctx)
{
    auto tok = r.peek();

    switch (tok.kind)
    {
    case Token.Kind.intLit:
        r.advance();
        return parseIntLit(tok);

    case Token.Kind.uintLit:
        r.advance();
        return parseUintLit(tok);

    case Token.Kind.floatLit:
        r.advance();
        return parseFloatLit(tok);

    case Token.Kind.stringLit:
        r.advance();
        return Value(parseStringContent(tok.text));

    case Token.Kind.bytesLit:
        r.advance();
        return Value(cast(immutable(ubyte)[]) parseStringContent(tok.text));

    case Token.Kind.trueKw:
        r.advance();
        return Value(true);

    case Token.Kind.falseKw:
        r.advance();
        return Value(false);

    case Token.Kind.nullKw:
        r.advance();
        return Value.null_();

    case Token.Kind.ident:
        r.advance();
        // Function call: ident(args...)
        if (r.peek().kind == Token.Kind.lparen)
        {
            r.advance(); // consume '('
            // Check macros first
            if (auto m = tok.text in env.macros)
            {
                return (*m)(r, env, ctx);
            }
            auto args = parseArgList(r, env, ctx);
            r.expect(Token.Kind.rparen);
            return evalFunction(tok.text, args, tok.pos);
        }
        // Variable lookup
        return ctx(tok.text);

    case Token.Kind.lparen:
        r.advance();
        auto inner = parseExpr(r, env, ctx, 0);
        r.expect(Token.Kind.rparen);
        return inner;

    case Token.Kind.lbracket:
        r.advance();
        return parseList(r, env, ctx);

    case Token.Kind.lbrace:
        r.advance();
        return parseMap(r, env, ctx);

    default:
        throw new EvalException("unexpected " ~ tok.toString(), tok.pos);
    }
}

/// Parse a ternary conditional (after consuming '?').
private Value parseTernary(ref TokenRange r, const Env env, Context ctx, Value cond)
{
    auto thenVal = parseExpr(r, env, ctx, 0);
    r.expect(Token.Kind.colon);
    auto elseVal = parseExpr(r, env, ctx, 0);
    if (cond.type == Value.Type.err)
        return cond;
    if (cond.type != Value.Type.bool_)
        return Value.err("ternary condition must be a bool, got " ~ typeName(cond));
    return isTruthy(cond) ? thenVal : elseVal;
}

/// Parse a comma-separated argument list (without consuming parens).
private Value[] parseArgList(ref TokenRange r, const Env env, Context ctx)
{
    Value[] args;
    if (r.peek().kind == Token.Kind.rparen)
        return args;
    args ~= parseExpr(r, env, ctx, 0);
    while (r.match(Token.Kind.comma))
        args ~= parseExpr(r, env, ctx, 0);
    return args;
}

/// Parse a list literal [...].
private Value parseList(ref TokenRange r, const Env env, Context ctx)
{
    Value[] elems;
    if (r.peek().kind == Token.Kind.rbracket)
    {
        r.advance();
        return Value(elems);
    }
    elems ~= parseExpr(r, env, ctx, 0);
    while (r.match(Token.Kind.comma))
    {
        if (r.peek().kind == Token.Kind.rbracket)
            break;
        elems ~= parseExpr(r, env, ctx, 0);
    }
    r.expect(Token.Kind.rbracket);
    return Value(elems);
}

/// Parse a map literal {...}.
private Value parseMap(ref TokenRange r, const Env env, Context ctx)
{
    Value[string] entries;
    if (r.peek().kind == Token.Kind.rbrace)
    {
        r.advance();
        return Value(entries);
    }

    parseMapEntry(r, env, ctx, entries);
    while (r.match(Token.Kind.comma))
    {
        if (r.peek().kind == Token.Kind.rbrace)
            break;
        parseMapEntry(r, env, ctx, entries);
    }
    r.expect(Token.Kind.rbrace);
    return Value(entries);
}

private void parseMapEntry(ref TokenRange r, const Env env, Context ctx, ref Value[string] entries)
{
    auto keyTok = r.peek();
    auto keyVal = parseExpr(r, env, ctx, 0);
    r.expect(Token.Kind.colon);
    auto valExpr = parseExpr(r, env, ctx, 0);

    auto keyStr = tryGet!string(keyVal);
    if (keyStr.isNull)
        throw new EvalException("map keys must be strings", keyTok.pos);
    entries[keyStr.get] = valExpr;
}

// ── Operator precedence ─────────────────────────────────────────────

private int infixPrec(Token.Kind kind)
{
    switch (kind)
    {
    case Token.Kind.question:
        return 1;
    case Token.Kind.pipePipe:
        return 2;
    case Token.Kind.ampAmp:
        return 3;
    case Token.Kind.eqEq:
    case Token.Kind.bangEq:
    case Token.Kind.inKw:
        return 4;
    case Token.Kind.lt:
    case Token.Kind.ltEq:
    case Token.Kind.gt:
    case Token.Kind.gtEq:
        return 5;
    case Token.Kind.plus:
    case Token.Kind.minus:
        return 6;
    case Token.Kind.star:
    case Token.Kind.slash:
    case Token.Kind.percent:
        return 7;
    case Token.Kind.dot:
    case Token.Kind.lbracket:
        return 8;
    default:
        return -1;
    }
}

// ── Evaluation helpers ──────────────────────────────────────────────

private bool isErr(Value v)
{
    return v.type == Value.Type.err;
}

/// Cross-type int×uint comparison helper.
/// Compares a long against a ulong for ordering operators.
/// When `swapped` is true, the original order was (uint, int) so comparison
/// direction must be reversed.
private Value crossCompare(long intVal, ulong uintVal, Token.Kind op, bool swapped)
{
    // Determine the relationship: intVal vs uintVal
    bool intLess;
    bool equal;
    if (intVal < 0)
    {
        intLess = true;
        equal = false;
    }
    else
    {
        auto asUlong = cast(ulong) intVal;
        intLess = asUlong < uintVal;
        equal = asUlong == uintVal;
    }

    // Without swap: expression is (int OP uint), so intLess means "lhs < rhs"
    // With swap: expression was (uint OP int), so we reverse the sense
    bool lhsLess = swapped ? (!intLess && !equal) : intLess;
    bool lhsEqual = equal;

    switch (op)
    {
    case Token.Kind.lt:
        return Value(lhsLess);
    case Token.Kind.ltEq:
        return Value(lhsLess || lhsEqual);
    case Token.Kind.gt:
        return Value(!lhsLess && !lhsEqual);
    case Token.Kind.gtEq:
        return Value(!lhsLess);
    default:
        return Value.err("unsupported comparison operator for int/uint");
    }
}

/// Cross-type int×uint binary operation.
/// When `swapped` is true, the original order was (uint, int) so comparison
/// direction must be reversed.
private Value crossNumericOp(long intVal, ulong uintVal, Token.Kind op, bool swapped = false)
{
    switch (op)
    {
    case Token.Kind.eqEq:
        if (intVal < 0)
            return Value(false);
        return Value(cast(ulong) intVal == uintVal);
    case Token.Kind.bangEq:
        if (intVal < 0)
            return Value(true);
        return Value(cast(ulong) intVal != uintVal);
    case Token.Kind.lt:
    case Token.Kind.ltEq:
    case Token.Kind.gt:
    case Token.Kind.gtEq:
        return crossCompare(intVal, uintVal, op, swapped);
        // Arithmetic: promote uint to long
    case Token.Kind.plus:
    case Token.Kind.minus:
    case Token.Kind.star:
    case Token.Kind.slash:
    case Token.Kind.percent:
        if (uintVal > cast(ulong) long.max)
            return Value.err("uint value too large for int arithmetic");
        if (swapped)
            return numericBinop!long(cast(long) uintVal, intVal, op);
        return numericBinop!long(intVal, cast(long) uintVal, op);
    default:
        return Value.err("unsupported operator for int/uint");
    }
}

/// Dispatch a binary op over two numeric values of the same type.
/// Handles arithmetic (+, -, *, /, %) and comparison (<, <=, >, >=, ==, !=).
private Value numericBinop(T)(T l, T r, Token.Kind op)
{
    switch (op)
    {
    case Token.Kind.plus:
        return Value(l + r);
    case Token.Kind.minus:
        return Value(l - r);
    case Token.Kind.star:
        return Value(l * r);
    case Token.Kind.slash:
        static if (is(T == double))
            return Value(l / r);
        else
        {
            if (r == 0)
                return Value.err("division by zero");
            return Value(l / r);
        }
    case Token.Kind.percent:
        static if (is(T == double))
            return Value(l % r);
        else
        {
            if (r == 0)
                return Value.err("modulo by zero");
            return Value(l % r);
        }
    case Token.Kind.eqEq:
        return Value(l == r);
    case Token.Kind.bangEq:
        return Value(l != r);
    case Token.Kind.lt:
        return Value(l < r);
    case Token.Kind.ltEq:
        return Value(l <= r);
    case Token.Kind.gt:
        return Value(l > r);
    case Token.Kind.gtEq:
        return Value(l >= r);
    default:
        return Value.err("unsupported operator " ~ kindName(op) ~ " for numeric types");
    }
}

private Value evalBinary(Token.Kind op, Value lhs, Value rhs)
{
    if (isErr(lhs))
        return lhs;
    if (isErr(rhs))
        return rhs;

    // Null equality: null == null → true, null != X → true
    if (lhs.type == Value.Type.null_ || rhs.type == Value.Type.null_)
    {
        if (op == Token.Kind.eqEq)
            return Value(lhs.type == Value.Type.null_ && rhs.type == Value.Type.null_);
        if (op == Token.Kind.bangEq)
            return Value(lhs.type != Value.Type.null_ || rhs.type != Value.Type.null_);
        return Value.err("unsupported operator " ~ kindName(op) ~ " for null");
    }

    // Try each numeric type pair via static foreach.
    alias NumericTypes = imported!"std.meta".AliasSeq!(long, ulong);
    static foreach (T; NumericTypes)
    {
        {
            auto l = tryGet!T(lhs);
            auto r = tryGet!T(rhs);
            if (!l.isNull && !r.isNull)
                return numericBinop!T(l.get, r.get, op);
        }
    }

    // Cross-type int×uint dispatch
    {
        auto li = tryGet!long(lhs);
        auto ru = tryGet!ulong(rhs);
        if (!li.isNull && !ru.isNull)
            return crossNumericOp(li.get, ru.get, op);
    }
    {
        auto lu = tryGet!ulong(lhs);
        auto ri = tryGet!long(rhs);
        if (!lu.isNull && !ri.isNull)
            return crossNumericOp(ri.get, lu.get, op, true); // swapped
    }

    // Duration binary ops: duration ± duration, duration comparison
    {
        auto ldur = tryGet!Duration(lhs);
        auto rdur = tryGet!Duration(rhs);
        if (!ldur.isNull && !rdur.isNull)
            return evalDurationBinary(ldur.get, rdur.get, op);
    }

    // Timestamp binary ops: ts - ts, ts ± dur, dur + ts, ts comparison
    if (lhs.type == Value.Type.timestamp || rhs.type == Value.Type.timestamp)
        return evalTimestampBinary(op, lhs, rhs);

    // Double (with int/uint promotion)
    auto ld = toDouble(lhs);
    auto rd = toDouble(rhs);
    if (!ld.isNull && !rd.isNull)
        return numericBinop!double(ld.get, rd.get, op);

    // String concatenation and comparison
    auto ls = tryGet!string(lhs);
    auto rs = tryGet!string(rhs);
    if (!ls.isNull && !rs.isNull)
    {
        switch (op)
        {
        case Token.Kind.plus:
            return Value(ls.get ~ rs.get);
        case Token.Kind.eqEq:
            return Value(ls.get == rs.get);
        case Token.Kind.bangEq:
            return Value(ls.get != rs.get);
        case Token.Kind.lt:
            return Value(ls.get < rs.get);
        case Token.Kind.ltEq:
            return Value(ls.get <= rs.get);
        case Token.Kind.gt:
            return Value(ls.get > rs.get);
        case Token.Kind.gtEq:
            return Value(ls.get >= rs.get);
        default:
            break;
        }
    }

    // Bool equality
    auto lb = tryGet!bool(lhs);
    auto rb = tryGet!bool(rhs);
    if (!lb.isNull && !rb.isNull)
    {
        switch (op)
        {
        case Token.Kind.eqEq:
            return Value(lb.get == rb.get);
        case Token.Kind.bangEq:
            return Value(lb.get != rb.get);
        default:
            break;
        }
    }

    // List concatenation
    auto ll = tryGet!(Value[])(lhs);
    auto rl = tryGet!(Value[])(rhs);
    if (!ll.isNull && !rl.isNull)
    {
        if (op == Token.Kind.plus)
            return Value(ll.get ~ rl.get);
    }

    return Value.err("unsupported operator " ~ kindName(
            op) ~ " for types " ~ typeName(lhs) ~ " and " ~ typeName(rhs));
}

private Value evalUnaryMinus(Value operand)
{
    if (isErr(operand))
        return operand;
    auto i = tryGet!long(operand);
    if (!i.isNull)
        return Value(-i.get);
    auto u = tryGet!ulong(operand);
    if (!u.isNull)
        return Value(-cast(long) u.get);
    auto d = tryGet!double(operand);
    if (!d.isNull)
        return Value(-d.get);
    return Value.err("cannot negate " ~ typeName(operand));
}

private Value evalUnaryNot(Value operand)
{
    if (isErr(operand))
        return operand;
    auto b = tryGet!bool(operand);
    if (!b.isNull)
        return Value(!b.get);
    return Value.err("cannot apply ! to " ~ typeName(operand));
}

private Value evalIn(Value lhs, Value rhs)
{
    if (isErr(lhs))
        return lhs;
    if (isErr(rhs))
        return rhs;

    auto ls = tryGet!string(lhs);
    auto rs = tryGet!string(rhs);
    if (!ls.isNull && !rs.isNull)
    {
        import std.algorithm : canFind;

        return Value(rs.get.canFind(ls.get));
    }

    auto rl = tryGet!(Value[])(rhs);
    if (!rl.isNull)
    {
        foreach (ref elem; rl.get)
            if (elem == lhs)
                return Value(true);
        return Value(false);
    }

    auto rm = tryGet!(Value[string])(rhs);
    if (!rm.isNull && !ls.isNull)
    {
        return Value((ls.get in rm.get) !is null);
    }

    return Value.err("unsupported 'in' for types " ~ typeName(lhs) ~ " and " ~ typeName(rhs));
}

private Value evalMember(Value obj, string name)
{
    if (isErr(obj))
        return obj;

    const m = tryGet!(Value[string])(obj);
    if (!m.isNull)
    {
        if (auto p = name in m.get)
            return *p;
        return Value.err("no such key: " ~ name);
    }
    auto e = tryGet!Entry(obj);
    if (!e.isNull)
        return e.get.resolve(name);
    return Value.err("cannot access member '" ~ name ~ "' on " ~ typeName(obj));
}

private Value evalIndex(Value obj, Value index)
{
    if (isErr(obj))
        return obj;
    if (isErr(index))
        return index;

    auto listVal = tryGet!(Value[])(obj);
    auto idxVal = tryGet!long(index);
    if (!listVal.isNull && !idxVal.isNull)
    {
        auto list = listVal.get;
        auto idx = idxVal.get;
        if (idx < 0)
            idx += cast(long) list.length;
        if (idx < 0 || idx >= cast(long) list.length)
            return Value.err("index out of range");
        return list[cast(size_t) idx];
    }

    const im = tryGet!(Value[string])(obj);
    auto ik = tryGet!string(index);
    if (!im.isNull && !ik.isNull)
    {
        if (auto p = ik.get in im.get)
            return *p;
        return Value.err("no such key: " ~ ik.get);
    }

    return Value.err("cannot index " ~ typeName(obj));
}

private Value evalFunction(string name, Value[] args, size_t pos)
{
    switch (name)
    {
    case "size":
        if (args.length != 1)
            throw new EvalException("size() takes exactly 1 argument", pos);
        return evalSize(args[0]);
    case "type":
        if (args.length != 1)
            throw new EvalException("type() takes exactly 1 argument", pos);
        if (isErr(args[0]))
            return args[0];
        return Value(typeName(args[0]));
    case "int":
        if (args.length != 1)
            throw new EvalException("int() takes exactly 1 argument", pos);
        return evalIntCast(args[0]);
    case "uint":
        if (args.length != 1)
            throw new EvalException("uint() takes exactly 1 argument", pos);
        return evalUintCast(args[0]);
    case "double":
        if (args.length != 1)
            throw new EvalException("double() takes exactly 1 argument", pos);
        return evalDoubleCast(args[0]);
    case "string":
        if (args.length != 1)
            throw new EvalException("string() takes exactly 1 argument", pos);
        return evalStringCast(args[0]);
    case "duration":
        if (args.length != 1)
            throw new EvalException("duration() takes exactly 1 argument", pos);
        return evalDurationCast(args[0]);
    case "timestamp":
        if (args.length != 1)
            throw new EvalException("timestamp() takes exactly 1 argument", pos);
        return evalTimestampCast(args[0]);
    default:
        throw new EvalException("unknown function: " ~ name, pos);
    }
}

private Value evalMethod(Value obj, string name, Value[] args)
{
    if (isErr(obj))
        return obj;

    switch (name)
    {
    case "size":
        if (args.length != 0)
            return Value.err(".size() takes no arguments");
        return evalSize(obj);
    case "contains":
        if (args.length != 1)
            return Value.err(".contains() takes exactly 1 argument");
        return evalContains(obj, args[0]);
    case "startsWith":
        if (args.length != 1)
            return Value.err(".startsWith() takes exactly 1 argument");
        return evalStartsWith(obj, args[0]);
    case "endsWith":
        if (args.length != 1)
            return Value.err(".endsWith() takes exactly 1 argument");
        return evalEndsWith(obj, args[0]);
    case "matches":
        if (args.length != 1)
            return Value.err(".matches() takes exactly 1 argument");
        return evalMatches(obj, args[0]);
    default:
        break;
    }

    // Duration accessor methods
    auto durVal = tryGet!Duration(obj);
    if (!durVal.isNull)
        return evalDurationMethod(durVal.get, name, args);

    // Timestamp accessor methods
    auto tsVal = tryGet!SysTime(obj);
    if (!tsVal.isNull)
        return evalTimestampMethod(tsVal.get, name, args);

    return Value.err("unknown method: " ~ name);
}

// ── Built-in function implementations ───────────────────────────────

private Value evalSize(Value v)
{
    if (isErr(v))
        return v;
    auto s = tryGet!string(v);
    if (!s.isNull)
        return Value(cast(long) s.get.length);
    auto b = tryGet!(immutable(ubyte)[])(v);
    if (!b.isNull)
        return Value(cast(long) b.get.length);
    auto l = tryGet!(Value[])(v);
    if (!l.isNull)
        return Value(cast(long) l.get.length);
    auto m = tryGet!(Value[string])(v);
    if (!m.isNull)
        return Value(cast(long) m.get.length);
    return Value.err("size() not supported for " ~ typeName(v));
}

private Value evalContains(Value obj, Value arg)
{
    import std.algorithm : canFind;

    if (isErr(obj))
        return obj;
    if (isErr(arg))
        return arg;
    auto s = tryGet!string(obj);
    auto sub = tryGet!string(arg);
    if (!s.isNull && !sub.isNull)
        return Value(s.get.canFind(sub.get));
    return Value.err(".contains() requires string arguments");
}

private Value evalStartsWith(Value obj, Value arg)
{
    import std.algorithm : startsWith;

    if (isErr(obj))
        return obj;
    if (isErr(arg))
        return arg;
    auto s = tryGet!string(obj);
    auto pre = tryGet!string(arg);
    if (!s.isNull && !pre.isNull)
        return Value(s.get.startsWith(pre.get));
    return Value.err(".startsWith() requires string arguments");
}

private Value evalMatches(Value obj, Value arg)
{
    import std.regex : regex, matchFirst, RegexException;

    if (isErr(obj))
        return obj;
    if (isErr(arg))
        return arg;
    auto s = tryGet!string(obj);
    auto pat = tryGet!string(arg);
    if (s.isNull || pat.isNull)
        return Value.err(".matches() requires string arguments");

    // CEL .matches() checks if the entire string matches the regex.
    // Anchor the pattern to enforce full-string matching.
    string anchored = "^(?:" ~ pat.get ~ ")$";

    try
    {
        auto re = regex(anchored);
        auto m = matchFirst(s.get, re);
        return Value(!m.empty);
    }
    catch (RegexException e)
    {
        return Value.err("invalid regex: " ~ e.msg);
    }
}

private Value evalEndsWith(Value obj, Value arg)
{
    import std.algorithm : endsWith;

    if (isErr(obj))
        return obj;
    if (isErr(arg))
        return arg;
    auto s = tryGet!string(obj);
    auto suf = tryGet!string(arg);
    if (!s.isNull && !suf.isNull)
        return Value(s.get.endsWith(suf.get));
    return Value.err(".endsWith() requires string arguments");
}

private Value evalIntCast(Value v)
{
    if (isErr(v))
        return v;
    auto i = tryGet!long(v);
    if (!i.isNull)
        return Value(i.get);
    auto u = tryGet!ulong(v);
    if (!u.isNull)
        return Value(cast(long) u.get);
    auto d = tryGet!double(v);
    if (!d.isNull)
        return Value(cast(long) d.get);
    auto s = tryGet!string(v);
    if (!s.isNull)
    {
        try
            return Value(s.get.to!long);
        catch (ConvException)
            return Value.err("cannot convert string to int: " ~ s.get);
    }
    return Value.err("cannot convert " ~ typeName(v) ~ " to int");
}

private Value evalUintCast(Value v)
{
    if (isErr(v))
        return v;
    auto i = tryGet!long(v);
    if (!i.isNull)
        return Value(cast(ulong) i.get);
    auto u = tryGet!ulong(v);
    if (!u.isNull)
        return Value(u.get);
    auto d = tryGet!double(v);
    if (!d.isNull)
        return Value(cast(ulong) d.get);
    auto s = tryGet!string(v);
    if (!s.isNull)
    {
        try
            return Value(s.get.to!ulong);
        catch (ConvException)
            return Value.err("cannot convert string to uint: " ~ s.get);
    }
    return Value.err("cannot convert " ~ typeName(v) ~ " to uint");
}

private Value evalDoubleCast(Value v)
{
    if (isErr(v))
        return v;
    auto i = tryGet!long(v);
    if (!i.isNull)
        return Value(cast(double) i.get);
    auto u = tryGet!ulong(v);
    if (!u.isNull)
        return Value(cast(double) u.get);
    auto d = tryGet!double(v);
    if (!d.isNull)
        return Value(d.get);
    auto s = tryGet!string(v);
    if (!s.isNull)
    {
        try
            return Value(s.get.to!double);
        catch (ConvException)
            return Value.err("cannot convert string to double: " ~ s.get);
    }
    return Value.err("cannot convert " ~ typeName(v) ~ " to double");
}

private Value evalStringCast(Value v)
{
    if (isErr(v))
        return v;
    auto s = tryGet!string(v);
    if (!s.isNull)
        return Value(s.get);
    auto i = tryGet!long(v);
    if (!i.isNull)
        return Value(i.get.to!string);
    auto u = tryGet!ulong(v);
    if (!u.isNull)
        return Value(u.get.to!string);
    auto d = tryGet!double(v);
    if (!d.isNull)
        return Value(d.get.to!string);
    auto b = tryGet!bool(v);
    if (!b.isNull)
        return Value(b.get ? "true" : "false");
    auto dur = tryGet!Duration(v);
    if (!dur.isNull)
        return Value(formatISO8601Duration(dur.get));
    auto ts = tryGet!SysTime(v);
    if (!ts.isNull)
        return Value(formatRFC3339(ts.get));
    return Value.err("cannot convert " ~ typeName(v) ~ " to string");
}

// ── Duration / Timestamp evaluation ─────────────────────────────────

private Value evalDurationCast(Value v)
{
    if (isErr(v))
        return v;
    auto d = tryGet!Duration(v);
    if (!d.isNull)
        return Value(d.get);
    auto s = tryGet!string(v);
    if (!s.isNull)
    {
        auto parsed = parseISO8601Duration(s.get);
        if (parsed.isNull)
            return Value.err("invalid duration string: " ~ s.get);
        return Value(parsed.get);
    }
    return Value.err("cannot convert " ~ typeName(v) ~ " to duration");
}

private Value evalTimestampCast(Value v)
{
    if (isErr(v))
        return v;
    auto t = tryGet!SysTime(v);
    if (!t.isNull)
        return Value(t.get);
    auto s = tryGet!string(v);
    if (!s.isNull)
    {
        auto parsed = parseRFC3339(s.get);
        if (parsed.isNull)
            return Value.err("invalid timestamp string: " ~ s.get);
        return Value(parsed.get);
    }
    return Value.err("cannot convert " ~ typeName(v) ~ " to timestamp");
}

/// Duration binary operations: duration ± duration, duration == duration, duration <=> duration.
private Value evalDurationBinary(Duration ld, Duration rd, Token.Kind op)
{
    switch (op)
    {
    case Token.Kind.plus:
        return Value(ld + rd);
    case Token.Kind.minus:
        return Value(ld - rd);
    case Token.Kind.eqEq:
        return Value(ld == rd);
    case Token.Kind.bangEq:
        return Value(ld != rd);
    case Token.Kind.lt:
        return Value(ld < rd);
    case Token.Kind.ltEq:
        return Value(ld <= rd);
    case Token.Kind.gt:
        return Value(ld > rd);
    case Token.Kind.gtEq:
        return Value(ld >= rd);
    default:
        return Value.err("unsupported operator " ~ kindName(op) ~ " for duration");
    }
}

/// Timestamp binary operations: ts - ts → duration, ts ± duration → ts, ts <=> ts.
private Value evalTimestampBinary(Token.Kind op, Value lhs, Value rhs)
{
    auto lt = tryGet!SysTime(lhs);
    auto rt = tryGet!SysTime(rhs);

    // timestamp - timestamp → duration
    if (!lt.isNull && !rt.isNull)
    {
        switch (op)
        {
        case Token.Kind.minus:
            return Value(lt.get - rt.get);
        case Token.Kind.eqEq:
            return Value(lt.get == rt.get);
        case Token.Kind.bangEq:
            return Value(lt.get != rt.get);
        case Token.Kind.lt:
            return Value(lt.get < rt.get);
        case Token.Kind.ltEq:
            return Value(lt.get <= rt.get);
        case Token.Kind.gt:
            return Value(lt.get > rt.get);
        case Token.Kind.gtEq:
            return Value(lt.get >= rt.get);
        default:
            return Value.err("unsupported operator " ~ kindName(op) ~ " for timestamps");
        }
    }

    // timestamp ± duration → timestamp
    if (!lt.isNull)
    {
        auto rd = tryGet!Duration(rhs);
        if (!rd.isNull)
        {
            if (op == Token.Kind.plus)
                return Value(lt.get + rd.get);
            if (op == Token.Kind.minus)
                return Value(lt.get - rd.get);
            return Value.err("unsupported operator " ~ kindName(op) ~ " for timestamp and duration");
        }
    }

    // duration + timestamp → timestamp
    auto ld = tryGet!Duration(lhs);
    if (!ld.isNull && !rt.isNull)
    {
        if (op == Token.Kind.plus)
            return Value(rt.get + ld.get);
        return Value.err("unsupported operator " ~ kindName(op) ~ " for duration and timestamp");
    }

    return Value.err("unsupported operand types for " ~ kindName(op));
}

/// Duration accessor methods.
private Value evalDurationMethod(Duration d, string name, Value[] args)
{
    if (args.length != 0)
        return Value.err("." ~ name ~ "() takes no arguments");
    switch (name)
    {
    case "hours":
        return Value(d.total!"hours");
    case "minutes":
        return Value(d.total!"minutes");
    case "seconds":
        return Value(d.total!"seconds");
    default:
        return Value.err("unknown duration method: " ~ name);
    }
}

/// Timestamp accessor methods.
private Value evalTimestampMethod(SysTime st, string name, Value[] args)
{
    if (args.length != 0)
        return Value.err("." ~ name ~ "() takes no arguments");
    auto utc = st.toUTC();
    switch (name)
    {
    case "year":
        return Value(cast(long) utc.year);
    case "month":
        return Value(cast(long) cast(int) utc.month);
    case "day":
        return Value(cast(long) utc.day);
    case "hour":
        return Value(cast(long) utc.hour);
    case "minute":
        return Value(cast(long) utc.minute);
    case "second":
        return Value(cast(long) utc.second);
    case "dayOfWeek":
        return Value(cast(long) cast(int) utc.dayOfWeek);
    case "dayOfYear":
        return Value(cast(long) utc.dayOfYear);
    default:
        return Value.err("unknown timestamp method: " ~ name);
    }
}

// ── Macros ──────────────────────────────────────────────────────────

/// Built-in macros that ship with every evaluator.
Macro[string] builtinMacros()
{
    Macro[string] m;
    m["has"] = (ref TokenRange r, const Env env, Context ctx) => macroHas(r, env, ctx);
    return m;
}

/// Built-in method macros for comprehensions.
MethodMacro[string] builtinMethodMacros()
{
    MethodMacro[string] m;
    m["all"] = (Value t, ref TokenRange r, const Env env, Context ctx) => macroAll(t, r, env, ctx);
    m["exists"] = (Value t, ref TokenRange r, const Env env, Context ctx) => macroExists(t,
            r, env, ctx);
    m["exists_one"] = (Value t, ref TokenRange r, const Env env, Context ctx) => macroExistsOne(t,
            r, env, ctx);
    m["map"] = (Value t, ref TokenRange r, const Env env, Context ctx) => macroMap(t, r, env, ctx);
    m["filter"] = (Value t, ref TokenRange r, const Env env, Context ctx) => macroFilter(t,
            r, env, ctx);
    return m;
}

/// Parsed comprehension: binder name and body token range.
private struct ComprehensionArgs
{
    /// Name of the iteration variable.
    string binder;
    /// Token position where the body expression starts.
    size_t bodyStart;
    /// Token position after the body expression.
    size_t bodyEnd;
}

/// Parse comprehension arguments: binder, comma, body expression.
/// Uses a probe sub-range with binder=null to find where the body ends.
private ComprehensionArgs parseComprehensionArgs(ref TokenRange r, const Env env, Context ctx)
{
    ComprehensionArgs args;
    auto binderTok = r.expect(Token.Kind.ident);
    args.binder = binderTok.text;
    r.expect(Token.Kind.comma);
    args.bodyStart = r.pos;
    // Probe: parse body with null binder to find extent
    auto probe = r.save();
    auto nullCtx = bindContext(ctx, args.binder, Value.null_());
    parseExpr(probe, env, nullCtx, 0);
    args.bodyEnd = probe.pos;
    // Advance main range to match
    r.pos = args.bodyEnd;
    r.expect(Token.Kind.rparen);
    return args;
}

/// Evaluate a comprehension body with a binder bound to element.
/// Creates a sub-range, parses, and asserts it lands at the expected end.
private Value evalBody(ref TokenRange r, const Env env, Context ctx,
        ComprehensionArgs args, Value element)
{
    auto sub = r.save();
    sub.pos = args.bodyStart;
    auto innerCtx = bindContext(ctx, args.binder, element);
    auto result = parseExpr(sub, env, innerCtx, 0);
    assert(sub.pos == args.bodyEnd, "comprehension body consumed different tokens on replay");
    return result;
}

/// Extract a list from a comprehension target, consuming args on error.
/// Returns null on error (with lhs set to the error Value to return).
private Nullable!(Value[]) extractListTarget(Value target, ref TokenRange r,
        const Env env, Context ctx)
{
    if (isErr(target))
    {
        cast(void) parseComprehensionArgs(r, env, ctx);
        return Nullable!(Value[]).init;
    }
    auto list = tryGet!(Value[])(target);
    if (list.isNull)
    {
        cast(void) parseComprehensionArgs(r, env, ctx);
        return Nullable!(Value[]).init;
    }
    return list;
}

/// `has(expr)` — true if expr doesn't produce an error.
private Value macroHas(ref TokenRange r, const Env env, Context ctx)
{
    auto arg = parseExpr(r, env, ctx, 0);
    r.expect(Token.Kind.rparen);
    return Value(arg.type != Value.Type.err);
}

/// `list.all(x, body)` — true if body is true for all elements.
private Value macroAll(Value target, ref TokenRange r, const Env env, Context ctx)
{
    auto list = extractListTarget(target, r, env, ctx);
    if (list.isNull)
        return isErr(target) ? target : Value.err(".all() requires a list");

    auto args = parseComprehensionArgs(r, env, ctx);
    foreach (ref elem; list.get)
    {
        auto result = evalBody(r, env, ctx, args, elem);
        if (isErr(result))
            return result;
        if (isFalsy(result))
            return Value(false);
    }
    return Value(true);
}

/// `list.exists(x, body)` — true if body is true for at least one element.
private Value macroExists(Value target, ref TokenRange r, const Env env, Context ctx)
{
    auto list = extractListTarget(target, r, env, ctx);
    if (list.isNull)
        return isErr(target) ? target : Value.err(".exists() requires a list");

    auto args = parseComprehensionArgs(r, env, ctx);
    foreach (ref elem; list.get)
    {
        auto result = evalBody(r, env, ctx, args, elem);
        if (isErr(result))
            return result;
        if (isTruthy(result))
            return Value(true);
    }
    return Value(false);
}

/// `list.exists_one(x, body)` — true if body is true for exactly one element.
private Value macroExistsOne(Value target, ref TokenRange r, const Env env, Context ctx)
{
    auto list = extractListTarget(target, r, env, ctx);
    if (list.isNull)
        return isErr(target) ? target : Value.err(".exists_one() requires a list");

    auto args = parseComprehensionArgs(r, env, ctx);
    int count = 0;
    foreach (ref elem; list.get)
    {
        auto result = evalBody(r, env, ctx, args, elem);
        if (isErr(result))
            return result;
        if (isTruthy(result))
            count++;
        if (count > 1)
            return Value(false);
    }
    return Value(count == 1);
}

/// `list.map(x, body)` — transform each element.
private Value macroMap(Value target, ref TokenRange r, const Env env, Context ctx)
{
    auto list = extractListTarget(target, r, env, ctx);
    if (list.isNull)
        return isErr(target) ? target : Value.err(".map() requires a list");

    auto args = parseComprehensionArgs(r, env, ctx);
    Value[] result;
    foreach (ref elem; list.get)
    {
        auto mapped = evalBody(r, env, ctx, args, elem);
        if (isErr(mapped))
            return mapped;
        result ~= mapped;
    }
    return Value(result);
}

/// `list.filter(x, body)` — keep elements where body is true.
private Value macroFilter(Value target, ref TokenRange r, const Env env, Context ctx)
{
    auto list = extractListTarget(target, r, env, ctx);
    if (list.isNull)
        return isErr(target) ? target : Value.err(".filter() requires a list");

    auto args = parseComprehensionArgs(r, env, ctx);
    Value[] result;
    foreach (ref elem; list.get)
    {
        auto pred = evalBody(r, env, ctx, args, elem);
        if (isErr(pred))
            return pred;
        if (isTruthy(pred))
            result ~= elem;
    }
    return Value(result);
}

// ── Literal parsing helpers ─────────────────────────────────────────

private Value parseIntLit(Token tok)
{
    try
    {
        if (tok.text.length > 2 && tok.text[0] == '0' && (tok.text[1] == 'x' || tok.text[1] == 'X'))
            return Value(tok.text[2 .. $].to!long(16));
        return Value(tok.text.to!long);
    }
    catch (ConvException)
    {
        throw new EvalException("invalid integer literal: " ~ tok.text, tok.pos);
    }
}

private Value parseUintLit(Token tok)
{
    try
    {
        auto text = tok.text[0 .. $ - 1]; // strip 'u'/'U' suffix
        if (text.length > 2 && text[0] == '0' && (text[1] == 'x' || text[1] == 'X'))
            return Value(text[2 .. $].to!ulong(16));
        return Value(text.to!ulong);
    }
    catch (ConvException)
    {
        throw new EvalException("invalid uint literal: " ~ tok.text, tok.pos);
    }
}

private Value parseFloatLit(Token tok)
{
    try
    {
        return Value(tok.text.to!double);
    }
    catch (ConvException)
    {
        throw new EvalException("invalid float literal: " ~ tok.text, tok.pos);
    }
}

private string parseStringContent(string raw)
{
    size_t prefixLen = 0;
    bool isRaw = false;
    foreach (idx, c; raw)
    {
        if (c == '"' || c == '\'')
        {
            prefixLen = idx;
            break;
        }
        if (c == 'r' || c == 'R')
            isRaw = true;
    }

    auto rest = raw[prefixLen .. $];
    const char quote = rest[0];

    const bool triple = rest.length >= 6 && rest[1] == quote && rest[2] == quote;
    auto content = triple ? rest[3 .. $ - 3] : rest[1 .. $ - 1];

    if (isRaw)
        return content;

    return processEscapes(content);
}

private string processEscapes(string s)
{
    char[] result;
    size_t i = 0;
    while (i < s.length)
    {
        if (s[i] == '\\' && i + 1 < s.length)
        {
            switch (s[i + 1])
            {
            case 'n':
                result ~= '\n';
                break;
            case 'r':
                result ~= '\r';
                break;
            case 't':
                result ~= '\t';
                break;
            case '\\':
                result ~= '\\';
                break;
            case '\'':
                result ~= '\'';
                break;
            case '"':
                result ~= '"';
                break;
            case '0':
                result ~= '\0';
                break;
            case 'a':
                result ~= '\a';
                break;
            case 'b':
                result ~= '\b';
                break;
            case 'f':
                result ~= '\f';
                break;
            case 'v':
                result ~= '\v';
                break;
            default:
                result ~= s[i .. i + 2];
                break;
            }
            i += 2;
        }
        else
        {
            result ~= s[i];
            i++;
        }
    }
    return cast(string) result;
}

// ── Duration / Timestamp helpers ────────────────────────────────────

/// Parse an ISO 8601 duration string (e.g., "PT1H30M", "PT30S", "P1DT12H").
/// Returns null on parse failure.
private Nullable!Duration parseISO8601Duration(string s)
{
    if (s.length < 2 || s[0] != 'P')
        return Nullable!Duration.init;

    size_t i = 1;
    long totalHnsecs = 0;
    bool inTime = false;

    while (i < s.length)
    {
        if (s[i] == 'T')
        {
            inTime = true;
            i++;
            continue;
        }

        // Parse number (may be fractional)
        size_t numStart = i;
        bool hasDot = false;
        while (i < s.length && ((s[i] >= '0' && s[i] <= '9') || s[i] == '.'))
        {
            if (s[i] == '.')
                hasDot = true;
            i++;
        }
        if (i >= s.length || i == numStart)
            return Nullable!Duration.init;

        char designator = s[i];
        i++;

        if (hasDot)
        {
            // Fractional — only makes sense for the last component (usually S)
            double val;
            try
                val = s[numStart .. i - 1].to!double;
            catch (ConvException)
                return Nullable!Duration.init;

            if (inTime && designator == 'S')
                totalHnsecs += cast(long)(val * 10_000_000);
            else if (inTime && designator == 'M')
                totalHnsecs += cast(long)(val * 60 * 10_000_000);
            else if (inTime && designator == 'H')
                totalHnsecs += cast(long)(val * 3600 * 10_000_000);
            else
                return Nullable!Duration.init;
        }
        else
        {
            long val;
            try
                val = s[numStart .. i - 1].to!long;
            catch (ConvException)
                return Nullable!Duration.init;

            if (!inTime && designator == 'D')
                totalHnsecs += val * 86_400 * 10_000_000;
            else if (inTime && designator == 'H')
                totalHnsecs += val * 3_600 * 10_000_000;
            else if (inTime && designator == 'M')
                totalHnsecs += val * 60 * 10_000_000;
            else if (inTime && designator == 'S')
                totalHnsecs += val * 10_000_000;
            else
                return Nullable!Duration.init;
        }
    }

    return nullable(dur!"hnsecs"(totalHnsecs));
}

/// Parse an RFC 3339 timestamp string (e.g., "2023-01-15T12:30:00Z").
/// Returns null on parse failure.
private Nullable!SysTime parseRFC3339(string s)
{
    import std.datetime.date : DateTime;

    try
    {
        // Minimal RFC 3339: YYYY-MM-DDThh:mm:ssZ or with offset
        if (s.length < 20)
            return Nullable!SysTime.init;

        int year = s[0 .. 4].to!int;
        if (s[4] != '-')
            return Nullable!SysTime.init;
        int month = s[5 .. 7].to!int;
        if (s[7] != '-')
            return Nullable!SysTime.init;
        int day = s[8 .. 10].to!int;
        if (s[10] != 'T' && s[10] != 't')
            return Nullable!SysTime.init;
        int hour = s[11 .. 13].to!int;
        if (s[13] != ':')
            return Nullable!SysTime.init;
        int minute = s[14 .. 16].to!int;
        if (s[16] != ':')
            return Nullable!SysTime.init;
        int second = s[17 .. 19].to!int;

        // Parse optional fractional seconds
        long fracHnsecs = 0;
        size_t i = 19;
        if (i < s.length && s[i] == '.')
        {
            i++;
            size_t fracStart = i;
            while (i < s.length && s[i] >= '0' && s[i] <= '9')
                i++;
            string fracStr = s[fracStart .. i];
            // Pad or truncate to 7 digits (hnsecs)
            while (fracStr.length < 7)
                fracStr ~= '0';
            if (fracStr.length > 7)
                fracStr = fracStr[0 .. 7];
            fracHnsecs = fracStr.to!long;
        }

        // Parse timezone: Z, +HH:MM, or -HH:MM
        import std.datetime.timezone : SimpleTimeZone;

        if (i >= s.length)
            return Nullable!SysTime.init;

        Duration tzOffset;
        if (s[i] == 'Z' || s[i] == 'z')
        {
            tzOffset = Duration.zero;
        }
        else if (s[i] == '+' || s[i] == '-')
        {
            if (i + 5 > s.length)
                return Nullable!SysTime.init;
            int tzHour = s[i + 1 .. i + 3].to!int;
            int tzMin = s[i + 4 .. i + 6].to!int;
            tzOffset = dur!"hours"(tzHour) + dur!"minutes"(tzMin);
            if (s[i] == '-')
                tzOffset = -tzOffset;
        }
        else
        {
            return Nullable!SysTime.init;
        }

        auto tz = cast(immutable) new SimpleTimeZone(tzOffset);

        import std.datetime.date : Month;

        auto dt = DateTime(year, cast(Month) month, day, hour, minute, second);
        auto st = SysTime(dt, dur!"hnsecs"(fracHnsecs), tz);
        return nullable(st);
    }
    catch (Exception)
    {
        return Nullable!SysTime.init;
    }
}

/// Format a Duration as an ISO 8601 duration string (e.g., "PT1H30M15S").
private string formatISO8601Duration(Duration d)
{
    import std.format : format;

    long totalSecs = d.total!"seconds";
    bool negative = totalSecs < 0;
    if (negative)
        totalSecs = -totalSecs;
    long rem = totalSecs;
    long h = rem / 3600;
    rem %= 3600;
    long m = rem / 60;
    long s = rem % 60;

    string result = negative ? "-PT" : "PT";
    if (h > 0)
        result ~= format!"%dH"(h);
    if (m > 0)
        result ~= format!"%dM"(m);
    // Always show seconds if nothing else, or if non-zero
    if (s > 0 || (h == 0 && m == 0))
        result ~= format!"%dS"(s);
    return result;
}

/// Format a SysTime as an RFC 3339 timestamp string.
private string formatRFC3339(SysTime st)
{
    import std.format : format;

    auto utc = st.toUTC();
    return format!"%04d-%02d-%02dT%02d:%02d:%02dZ"(utc.year,
            cast(int) utc.month, utc.day, utc.hour, utc.minute, utc.second);
}

// ── Value helpers ───────────────────────────────────────────────────

private Nullable!T tryGet(T)(Value v)
{
    return v.inner.match!((ref T val) => nullable(val), (ref _) => Nullable!T.init,);
}

private Nullable!double toDouble(Value v)
{
    return v.inner.match!((ref double d) => nullable(d),
            (ref long i) => nullable(cast(double) i),
            (ref ulong u) => nullable(cast(double) u), (ref _) => Nullable!(double).init,);
}

/// True only for `Value(true)`. Non-bool values are NOT truthy.
private bool isTruthy(Value v)
{
    return v.inner.match!((ref bool b) => b, (ref _) => false,);
}

/// True only for `Value(false)`. Non-bool values are NOT falsy.
private bool isFalsy(Value v)
{
    return v.inner.match!((ref bool b) => !b, (ref _) => false,);
}

/// Coerce a value to bool, returning an error for non-bool types.
private Value toBool(Value v)
{
    if (v.type == Value.Type.bool_ || v.type == Value.Type.err)
        return v;
    return Value.err("expected bool, got " ~ typeName(v));
}

private string typeName(Value v)
{
    final switch (v.type)
    {
    case Value.Type.null_:
        return "null";
    case Value.Type.bool_:
        return "bool";
    case Value.Type.int_:
        return "int";
    case Value.Type.uint_:
        return "uint";
    case Value.Type.double_:
        return "double";
    case Value.Type.string_:
        return "string";
    case Value.Type.bytes_:
        return "bytes";
    case Value.Type.list:
        return "list";
    case Value.Type.map:
        return "map";
    case Value.Type.entry:
        return "entry";
    case Value.Type.duration:
        return "google.protobuf.Duration";
    case Value.Type.timestamp:
        return "google.protobuf.Timestamp";
    case Value.Type.err:
        return "error";
    }
}

// ── Tests ───────────────────────────────────────────────────────────

@("Eval: integer arithmetic")
unittest
{
    import dshould;

    evaluate("1 + 2", emptyContext()).should.be(value(3L));
    evaluate("10 - 3", emptyContext()).should.be(value(7L));
    evaluate("4 * 5", emptyContext()).should.be(value(20L));
    evaluate("15 / 4", emptyContext()).should.be(value(3L));
    evaluate("15 % 4", emptyContext()).should.be(value(3L));
}

@("Eval: operator precedence")
unittest
{
    import dshould;

    evaluate("2 + 3 * 4", emptyContext()).should.be(value(14L));
    evaluate("(2 + 3) * 4", emptyContext()).should.be(value(20L));
}

@("Eval: comparison operators")
unittest
{
    import dshould;

    evaluate("1 < 2", emptyContext()).should.be(value(true));
    evaluate("2 <= 2", emptyContext()).should.be(value(true));
    evaluate("3 > 2", emptyContext()).should.be(value(true));
    evaluate("2 >= 3", emptyContext()).should.be(value(false));
    evaluate("1 == 1", emptyContext()).should.be(value(true));
    evaluate("1 != 2", emptyContext()).should.be(value(true));
}

@("Eval: logical operators")
unittest
{
    import dshould;

    evaluate("true && true", emptyContext()).should.be(value(true));
    evaluate("true && false", emptyContext()).should.be(value(false));
    evaluate("false || true", emptyContext()).should.be(value(true));
    evaluate("false || false", emptyContext()).should.be(value(false));
    evaluate("!true", emptyContext()).should.be(value(false));
    evaluate("!false", emptyContext()).should.be(value(true));
}

@("Eval: logical operators reject non-bool operands")
unittest
{
    import dshould;

    evaluate("1 && true", emptyContext()).type.should.be(Value.Type.err);
    evaluate(`"yes" && true`, emptyContext()).type.should.be(Value.Type.err);
    // Short-circuit still absorbs: false && <non-bool> → false, true || <non-bool> → true
    evaluate("false && 1", emptyContext()).should.be(value(false));
    evaluate("true || 1", emptyContext()).should.be(value(true));
    // Non-short-circuit path rejects non-bool RHS
    evaluate("true && 1", emptyContext()).type.should.be(Value.Type.err);
    evaluate("false || 1", emptyContext()).type.should.be(Value.Type.err);
}

@("Eval: string operations")
unittest
{
    import dshould;

    evaluate(`"hello" + " " + "world"`, emptyContext()).should.be(value("hello world"));
    evaluate(`"abc" == "abc"`, emptyContext()).should.be(value(true));
    evaluate(`"abc" != "def"`, emptyContext()).should.be(value(true));
    evaluate(`"abc" < "def"`, emptyContext()).should.be(value(true));
}

@("Eval: string methods")
unittest
{
    import dshould;

    evaluate(`"hello world".contains("world")`, emptyContext()).should.be(value(true));
    evaluate(`"hello world".startsWith("hello")`, emptyContext()).should.be(value(true));
    evaluate(`"hello world".endsWith("world")`, emptyContext()).should.be(value(true));
    evaluate(`"hello".size()`, emptyContext()).should.be(value(5L));
}

@("Eval: context variable lookup")
unittest
{
    import dshould;

    auto ctx = contextFrom(["x": value(10L), "name": value("alice")]);
    evaluate("x", ctx).should.be(value(10L));
    evaluate("name", ctx).should.be(value("alice"));
    evaluate("x + 5", ctx).should.be(value(15L));
}

@("Eval: ternary conditional")
unittest
{
    import dshould;

    evaluate("true ? 1 : 2", emptyContext()).should.be(value(1L));
    evaluate("false ? 1 : 2", emptyContext()).should.be(value(2L));
}

@("Eval: ternary rejects non-bool condition")
unittest
{
    import dshould;

    evaluate("1 ? 2 : 3", emptyContext()).type.should.be(Value.Type.err);
    evaluate(`"yes" ? 1 : 0`, emptyContext()).type.should.be(Value.Type.err);
}

@("Eval: list literals and indexing")
unittest
{
    import dshould;

    evaluate("[1, 2, 3][0]", emptyContext()).should.be(value(1L));
    evaluate("[1, 2, 3][2]", emptyContext()).should.be(value(3L));
    evaluate("size([1, 2, 3])", emptyContext()).should.be(value(3L));
}

@("Eval: map literals and access")
unittest
{
    import dshould;

    evaluate(`{"a": 1, "b": 2}.a`, emptyContext()).should.be(value(1L));
    evaluate(`{"a": 1, "b": 2}["b"]`, emptyContext()).should.be(value(2L));
}

@("Eval: unary minus")
unittest
{
    import dshould;

    evaluate("-5", emptyContext()).should.be(value(-5L));
    evaluate("-3.14", emptyContext()).should.be(value(-3.14));
    evaluate("-(2 + 3)", emptyContext()).should.be(value(-5L));
}

@("Eval: float arithmetic")
unittest
{
    import dshould;

    evaluate("1.5 + 2.5", emptyContext()).should.be(value(4.0));
    evaluate("3.0 * 2.0", emptyContext()).should.be(value(6.0));
}

@("Eval: type casts")
unittest
{
    import dshould;

    evaluate(`int("42")`, emptyContext()).should.be(value(42L));
    evaluate("double(42)", emptyContext()).should.be(value(42.0));
    evaluate(`string(42)`, emptyContext()).should.be(value("42"));
}

@("Eval: in operator")
unittest
{
    import dshould;

    evaluate(`"el" in "hello"`, emptyContext()).should.be(value(true));
    evaluate(`"x" in {"x": 1, "y": 2}`, emptyContext()).should.be(value(true));
    evaluate(`"z" in {"x": 1, "y": 2}`, emptyContext()).should.be(value(false));
}

@("Eval: division by zero returns error value")
unittest
{
    import dshould;

    evaluate("1 / 0", emptyContext()).type.should.be(Value.Type.err);
}

@("Eval: syntax error throws")
unittest
{
    import dshould;

    evaluate("1 +", emptyContext()).should.throwA!EvalException;
    evaluate("", emptyContext()).should.throwA!EvalException;
}

@("Eval: unknown variable from context")
unittest
{
    import dshould;

    auto result = evaluate("x", emptyContext());
    result.type.should.be(Value.Type.err);
}

@("Eval: nested member access")
unittest
{
    import dshould;

    auto ctx = contextFrom([
        "req": value(["method": value("GET"), "host": value("example.com")])
    ]);
    evaluate(`req.method`, ctx).should.be(value("GET"));
    evaluate(`req.host == "example.com"`, ctx).should.be(value(true));
}

@("Eval: short-circuit evaluation")
unittest
{
    import dshould;

    evaluate("false && (1/0 == 1)", emptyContext()).should.be(value(false));
    evaluate("true || (1/0 == 1)", emptyContext()).should.be(value(true));
}

@("Eval: has() macro")
unittest
{
    import dshould;

    auto ctx = contextFrom([
        "req": value(["method": value("GET"), "path": value("/api")])
    ]);
    evaluate(`has(req.method)`, ctx).should.be(value(true));
    evaluate(`has(req.missing)`, ctx).should.be(value(false));
    evaluate(`has(req)`, ctx).should.be(value(true));
    evaluate(`has(unknown)`, ctx).should.be(value(false));
}

@("Eval: in operator with list membership")
unittest
{
    import dshould;

    evaluate(`1 in [1, 2, 3]`, emptyContext()).should.be(value(true));
    evaluate(`4 in [1, 2, 3]`, emptyContext()).should.be(value(false));
    evaluate(`"a" in ["a", "b", "c"]`, emptyContext()).should.be(value(true));
    evaluate(`"d" in ["a", "b", "c"]`, emptyContext()).should.be(value(false));
}

@("Eval: custom macros")
unittest
{
    import dshould;

    Macro[string] customs;
    customs["always"] = delegate Value(ref TokenRange r, const Env env, Context ctx) {
        parseExpr(r, env, ctx, 0); // evaluate and discard
        r.expect(Token.Kind.rparen);
        return Value(true);
    };
    evaluateWithMacros(`always(1 + 2)`, emptyContext(), customs).should.be(value(true));
}

@("Eval: list.all() comprehension")
unittest
{
    import dshould;

    evaluate("[1, 2, 3].all(x, x > 0)", emptyContext()).should.be(value(true));
    evaluate("[1, 2, 3].all(x, x > 2)", emptyContext()).should.be(value(false));
    evaluate("[].all(x, x > 0)", emptyContext()).should.be(value(true));
}

@("Eval: list.exists() comprehension")
unittest
{
    import dshould;

    evaluate("[1, 2, 3].exists(x, x == 2)", emptyContext()).should.be(value(true));
    evaluate("[1, 2, 3].exists(x, x > 5)", emptyContext()).should.be(value(false));
    evaluate("[].exists(x, x > 0)", emptyContext()).should.be(value(false));
}

@("Eval: list.exists_one() comprehension")
unittest
{
    import dshould;

    evaluate("[1, 2, 3].exists_one(x, x == 2)", emptyContext()).should.be(value(true));
    evaluate("[1, 2, 3].exists_one(x, x > 1)", emptyContext()).should.be(value(false));
    evaluate("[1, 2, 3].exists_one(x, x > 5)", emptyContext()).should.be(value(false));
}

@("Eval: list.map() comprehension")
unittest
{
    import dshould;

    evaluate("[1, 2, 3].map(x, x * 2)", emptyContext()).should.be(value([
        value(2L), value(4L), value(6L)
    ]));
    evaluate("[].map(x, x + 1)", emptyContext()).should.be(value(cast(Value[])[]));
}

@("Eval: list.filter() comprehension")
unittest
{
    import dshould;

    evaluate("[1, 2, 3, 4, 5].filter(x, x > 3)", emptyContext()).should.be(
            value([value(4L), value(5L)]));
    evaluate("[1, 2, 3].filter(x, x > 5)", emptyContext()).should.be(value(cast(Value[])[
    ]));
}

@("Eval: comprehension with context variables")
unittest
{
    import dshould;

    auto ctx = contextFrom(["threshold": value(2L)]);
    evaluate("[1, 2, 3, 4].filter(x, x > threshold)", ctx).should.be(value([
        value(3L), value(4L)
    ]));
    evaluate("[1, 2, 3].all(x, x > threshold)", ctx).should.be(value(false));
    evaluate("[3, 4, 5].all(x, x > threshold)", ctx).should.be(value(true));
}

@("Eval: chained comprehensions")
unittest
{
    import dshould;

    evaluate("[1, 2, 3, 4].map(x, x * 2).filter(y, y > 4)", emptyContext()).should.be(
            value([value(6L), value(8L)]));
}

@("Eval: error propagation")
unittest
{
    import dshould;

    evaluate("1/0 + 1", emptyContext()).type.should.be(Value.Type.err);
    evaluate("-(1/0)", emptyContext()).type.should.be(Value.Type.err);
    evaluate("true || (1/0 == 1)", emptyContext()).should.be(value(true));
    evaluate("false && (1/0 == 1)", emptyContext()).should.be(value(false));
    evaluate("true && (1/0 == 1)", emptyContext()).type.should.be(Value.Type.err);
    evaluate("false || (1/0 == 1)", emptyContext()).type.should.be(Value.Type.err);
}

@("Eval: .matches() regex method")
unittest
{
    import dshould;

    // Basic matching
    evaluate(`"hello".matches("hel.*")`, emptyContext()).should.be(value(true));
    evaluate(`"hello".matches("world")`, emptyContext()).should.be(value(false));

    // Full string match (not partial)
    evaluate(`"hello world".matches("hello")`, emptyContext()).should.be(value(false));
    evaluate(`"hello world".matches("hello.*")`, emptyContext()).should.be(value(true));

    // Character classes
    evaluate(`"abc123".matches("[a-z]+[0-9]+")`, emptyContext()).should.be(value(true));

    // Empty string
    evaluate(`"".matches("")`, emptyContext()).should.be(value(true));
    evaluate(`"".matches(".")`, emptyContext()).should.be(value(false));
}

@("Eval: null semantics")
unittest
{
    import dshould;

    // null == null → true
    evaluate("null == null", emptyContext()).should.be(value(true));
    // null != non-null → true
    evaluate("null != 1", emptyContext()).should.be(value(true));
    evaluate("1 != null", emptyContext()).should.be(value(true));
    evaluate(`null != "x"`, emptyContext()).should.be(value(true));
    // null == non-null → false
    evaluate("null == 1", emptyContext()).should.be(value(false));
    evaluate("1 == null", emptyContext()).should.be(value(false));
    // null arithmetic → error
    evaluate("null + 1", emptyContext()).type.should.be(Value.Type.err);
    evaluate("null < 1", emptyContext()).type.should.be(Value.Type.err);
}

@("Eval: cross-type numeric comparison")
unittest
{
    import dshould;

    // int vs uint equality
    evaluate("1u == 1", emptyContext()).should.be(value(true));
    evaluate("1 == 1u", emptyContext()).should.be(value(true));
    evaluate("1u != 2", emptyContext()).should.be(value(true));
    evaluate("1u != 1", emptyContext()).should.be(value(false));

    // int vs uint ordering
    evaluate("1u < 2", emptyContext()).should.be(value(true));
    evaluate("2u > 1", emptyContext()).should.be(value(true));
    evaluate("1u <= 1", emptyContext()).should.be(value(true));
    evaluate("1u >= 1", emptyContext()).should.be(value(true));
    evaluate("3u > 2", emptyContext()).should.be(value(true));
    evaluate("1 < 2u", emptyContext()).should.be(value(true));
    evaluate("2 > 1u", emptyContext()).should.be(value(true));
    evaluate("1 <= 1u", emptyContext()).should.be(value(true));
    evaluate("1 >= 1u", emptyContext()).should.be(value(true));

    // negative int vs uint
    evaluate("-1 < 1u", emptyContext()).should.be(value(true));
    evaluate("-1 == 1u", emptyContext()).should.be(value(false));
    evaluate("-1 != 1u", emptyContext()).should.be(value(true));
    evaluate("-1 <= 1u", emptyContext()).should.be(value(true));
    evaluate("-1 > 1u", emptyContext()).should.be(value(false));
    evaluate("-1 >= 1u", emptyContext()).should.be(value(false));

    // cross-type arithmetic
    evaluate("1u + 1", emptyContext()).type.should.not.be(Value.Type.err);
    evaluate("3u - 1", emptyContext()).type.should.not.be(Value.Type.err);
    evaluate("2u * 3", emptyContext()).type.should.not.be(Value.Type.err);
    evaluate("1 + 1u", emptyContext()).type.should.not.be(Value.Type.err);
    evaluate("3 - 1u", emptyContext()).type.should.not.be(Value.Type.err);

    // double promotion from either type
    evaluate("1.0 == 1", emptyContext()).should.be(value(true));
    evaluate("1.0 == 1u", emptyContext()).should.be(value(true));
    evaluate("1 + 1.5", emptyContext()).should.be(value(2.5));
    evaluate("1u + 1.5", emptyContext()).should.be(value(2.5));
}

@("Eval: duration constructor and methods")
unittest
{
    import dshould;

    // Parse ISO 8601 durations
    evaluate(`duration("PT30S")`, emptyContext()).type.should.be(Value.Type.duration);
    evaluate(`duration("PT1H30M")`, emptyContext()).type.should.be(Value.Type.duration);
    evaluate(`duration("PT1H")`, emptyContext()).type.should.be(Value.Type.duration);
    evaluate(`duration("P1DT12H")`, emptyContext()).type.should.be(Value.Type.duration);

    // Accessor methods
    evaluate(`duration("PT1H30M").hours()`, emptyContext()).should.be(value(1L));
    evaluate(`duration("PT1H30M").minutes()`, emptyContext()).should.be(value(90L));
    evaluate(`duration("PT1H30M").seconds()`, emptyContext()).should.be(value(5400L));
    evaluate(`duration("PT90S").minutes()`, emptyContext()).should.be(value(1L));
    evaluate(`duration("PT90S").seconds()`, emptyContext()).should.be(value(90L));

    // Invalid duration → error
    evaluate(`duration("not a duration")`, emptyContext()).type.should.be(Value.Type.err);
    evaluate(`duration(42)`, emptyContext()).type.should.be(Value.Type.err);
}

@("Eval: duration arithmetic")
unittest
{
    import dshould;

    // duration + duration
    evaluate(`duration("PT1H") + duration("PT30M")`, emptyContext()).should.be(
            evaluate(`duration("PT1H30M")`, emptyContext()));

    // duration - duration
    evaluate(`duration("PT1H") - duration("PT30M")`, emptyContext()).should.be(
            evaluate(`duration("PT30M")`, emptyContext()));

    // duration comparison
    evaluate(`duration("PT1H") > duration("PT30M")`, emptyContext()).should.be(value(true));
    evaluate(`duration("PT1H") < duration("PT30M")`, emptyContext()).should.be(value(false));
    evaluate(`duration("PT1H") == duration("PT1H")`, emptyContext()).should.be(value(true));
    evaluate(`duration("PT1H") != duration("PT30M")`, emptyContext()).should.be(value(true));
    evaluate(`duration("PT1H") >= duration("PT1H")`, emptyContext()).should.be(value(true));
    evaluate(`duration("PT30M") <= duration("PT1H")`, emptyContext()).should.be(value(true));
}

@("Eval: timestamp constructor and methods")
unittest
{
    import dshould;

    // Parse RFC 3339 timestamps
    evaluate(`timestamp("2023-01-15T12:30:00Z")`, emptyContext()).type.should.be(
            Value.Type.timestamp);

    // Accessor methods
    evaluate(`timestamp("2023-01-15T12:30:45Z").year()`, emptyContext()).should.be(value(2023L));
    evaluate(`timestamp("2023-01-15T12:30:45Z").month()`, emptyContext()).should.be(value(1L));
    evaluate(`timestamp("2023-01-15T12:30:45Z").day()`, emptyContext()).should.be(value(15L));
    evaluate(`timestamp("2023-01-15T12:30:45Z").hour()`, emptyContext()).should.be(value(12L));
    evaluate(`timestamp("2023-01-15T12:30:45Z").minute()`, emptyContext()).should.be(value(30L));
    evaluate(`timestamp("2023-01-15T12:30:45Z").second()`, emptyContext()).should.be(value(45L));

    // Invalid timestamp → error
    evaluate(`timestamp("not a timestamp")`, emptyContext()).type.should.be(Value.Type.err);
    evaluate(`timestamp(42)`, emptyContext()).type.should.be(Value.Type.err);
}

@("Eval: timestamp arithmetic")
unittest
{
    import dshould;

    // timestamp + duration
    evaluate(`timestamp("2023-01-15T12:00:00Z") + duration("PT1H")`, emptyContext()).should.be(
            evaluate(`timestamp("2023-01-15T13:00:00Z")`, emptyContext()));

    // timestamp - duration
    evaluate(`timestamp("2023-01-15T12:00:00Z") - duration("PT1H")`, emptyContext()).should.be(
            evaluate(`timestamp("2023-01-15T11:00:00Z")`, emptyContext()));

    // duration + timestamp (commutative)
    evaluate(`duration("PT1H") + timestamp("2023-01-15T12:00:00Z")`, emptyContext()).should.be(
            evaluate(`timestamp("2023-01-15T13:00:00Z")`, emptyContext()));

    // timestamp comparison
    evaluate(`timestamp("2023-01-15T12:00:00Z") < timestamp("2023-01-15T13:00:00Z")`,
            emptyContext()).should.be(value(true));
    evaluate(`timestamp("2023-01-15T12:00:00Z") == timestamp("2023-01-15T12:00:00Z")`,
            emptyContext()).should.be(value(true));
    evaluate(`timestamp("2023-01-15T13:00:00Z") > timestamp("2023-01-15T12:00:00Z")`,
            emptyContext()).should.be(value(true));
}

@("Eval: timestamp - timestamp → duration")
unittest
{
    import dshould;

    auto result = evaluate(`timestamp("2023-01-15T13:00:00Z") - timestamp("2023-01-15T12:00:00Z")`,
            emptyContext());
    result.type.should.be(Value.Type.duration);
    // The resulting duration should be 1 hour
    evaluate(`(timestamp("2023-01-15T13:00:00Z") - timestamp("2023-01-15T12:00:00Z")).seconds()`,
            emptyContext()).should.be(value(3600L));
}

@("Eval: duration and timestamp string() cast")
unittest
{
    import dshould;

    evaluate(`string(duration("PT1H30M"))`, emptyContext()).should.be(value("PT1H30M"));
    evaluate(`string(duration("PT30S"))`, emptyContext()).should.be(value("PT30S"));
    evaluate(`string(timestamp("2023-01-15T12:30:00Z"))`, emptyContext()).should.be(
            value("2023-01-15T12:30:00Z"));
}

@("Eval: type() for duration and timestamp")
unittest
{
    import dshould;

    evaluate(`type(duration("PT1H"))`, emptyContext()).should.be(value("google.protobuf.Duration"));
    evaluate(`type(timestamp("2023-01-15T12:00:00Z"))`, emptyContext()).should.be(
            value("google.protobuf.Timestamp"));
}

@("Eval: duration/timestamp from context")
unittest
{
    import dshould;
    import core.time : hours, minutes;
    import std.datetime.systime : SysTime;
    import std.datetime.date : DateTime;
    import std.datetime.timezone : UTC;

    auto ctx = contextFrom([
        "timeout": value(dur!"seconds"(30)),
        "created": value(SysTime(DateTime(2023, 1, 15, 12, 0, 0), UTC())),
    ]);
    evaluate("timeout.seconds()", ctx).should.be(value(30L));
    evaluate("created.year()", ctx).should.be(value(2023L));
    evaluate(`created + duration("PT1H")`, ctx).type.should.be(Value.Type.timestamp);
}
