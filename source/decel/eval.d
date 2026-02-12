/++
 + CEL Evaluator — single-pass recursive descent interpreter.
 +
 + Tokenizes and evaluates a CEL expression in one pass, producing a Value.
 + Uses Pratt parsing for correct operator precedence.
 +
 + Evaluation errors (division by zero, type mismatches) are represented as
 + Value.err rather than exceptions. This enables natural short-circuit
 + semantics: `false && (1/0 == 1)` evaluates to `false` because `&&`
 + absorbs the error on the right when the left is already false.
 +
 + Parse errors (syntax errors, unexpected tokens) still throw EvalException
 + since the parser cannot continue.
 +/
module decel.eval;

import decel.context;
import decel.lexer;
import decel.value;

import std.conv : to, ConvException;
import std.sumtype : match;

/++
 + Exception thrown for parse or evaluation errors.
 + Carries the byte offset in the source where the error occurred.
 +/
class EvalException : Exception
{
    /// Byte offset in the source where the error occurred.
    size_t position;

    /// Construct an EvalException with a message and source position.
    this(string msg, size_t pos, string file = __FILE__, size_t line = __LINE__)
    {
        import std.format : format;

        super(format!"at position %d: %s"(pos, msg), file, line);
        position = pos;
    }
}

/// Evaluate a CEL expression string against a context.
Value evaluate(string source, Context ctx)
{
    auto tokens = tokenize(source);
    auto parser = Parser(tokens, ctx);
    auto result = parser.parseExpr(0);
    parser.expect(Token.Kind.eof);
    return result;
}

// ── Pratt parser / evaluator ────────────────────────────────────────

private struct Parser
{
    Token[] tokens;
    Context ctx;
    size_t pos;

    this(Token[] tokens, Context ctx)
    {
        this.tokens = tokens;
        this.ctx = ctx;
        this.pos = 0;
    }

    /// Current token.
    Token peek()
    {
        return tokens[pos];
    }

    /// Advance and return the consumed token.
    Token advance()
    {
        auto tok = tokens[pos];
        if (tok.kind != Token.Kind.eof)
            pos++;
        return tok;
    }

    /// Consume a token of the expected kind, or throw.
    Token expect(Token.Kind kind)
    {
        auto tok = peek();
        if (tok.kind != kind)
            throw new EvalException("expected " ~ kindName(kind) ~ ", got " ~ tok.toString(),
                    tok.pos);
        return advance();
    }

    /// Try to consume a token of the given kind. Returns true if consumed.
    bool match(Token.Kind kind)
    {
        if (peek().kind == kind)
        {
            advance();
            return true;
        }
        return false;
    }

    // ── Expression parsing (Pratt) ──────────────────────────────────

    /// Parse an expression with the given minimum precedence.
    Value parseExpr(int minPrec)
    {
        auto lhs = parseUnary();

        while (true)
        {
            auto tok = peek();
            auto prec = infixPrec(tok.kind);
            if (prec < 0 || prec < minPrec)
                break;

            // Ternary conditional: expr ? expr : expr
            if (tok.kind == Token.Kind.question)
            {
                advance();
                lhs = parseTernary(lhs);
                continue;
            }

            // `in` operator
            if (tok.kind == Token.Kind.inKw)
            {
                advance();
                auto rhs = parseExpr(prec + 1);
                lhs = evalIn(lhs, rhs);
                continue;
            }

            // Member access: expr.ident
            if (tok.kind == Token.Kind.dot)
            {
                advance();
                auto ident = expect(Token.Kind.ident);

                // Method call: expr.ident(args...)
                if (peek().kind == Token.Kind.lparen)
                {
                    advance();
                    auto args = parseArgList();
                    expect(Token.Kind.rparen);
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
                advance();
                auto index = parseExpr(0);
                expect(Token.Kind.rbracket);
                lhs = evalIndex(lhs, index);
                continue;
            }

            // Binary operator
            advance();

            // Short-circuit: && and ||
            // Both sides are always fully parsed. Error values propagate
            // naturally: false && err → false, true || err → true.
            if (tok.kind == Token.Kind.ampAmp)
            {
                auto rhs = parseExpr(prec + 1);
                // false && <anything> → false (absorb RHS errors)
                if (isFalsy(lhs))
                    continue; // lhs stays as-is (false)
                // true && rhs → rhs
                lhs = rhs;
                continue;
            }
            if (tok.kind == Token.Kind.pipePipe)
            {
                auto rhs = parseExpr(prec + 1);
                // true || <anything> → true (absorb RHS errors)
                if (isTruthy(lhs))
                    continue; // lhs stays as-is (true)
                // false || rhs → rhs
                lhs = rhs;
                continue;
            }

            auto rhs = parseExpr(prec + 1);
            lhs = evalBinary(tok.kind, lhs, rhs);
        }

        return lhs;
    }

    /// Parse a unary expression (prefix operators and atoms).
    Value parseUnary()
    {
        auto tok = peek();

        // Unary negation
        if (tok.kind == Token.Kind.minus)
        {
            advance();
            auto operand = parseUnary();
            return evalUnaryMinus(operand);
        }

        // Logical not
        if (tok.kind == Token.Kind.bang)
        {
            advance();
            auto operand = parseUnary();
            return evalUnaryNot(operand);
        }

        return parseAtom();
    }

    /// Parse an atomic expression (literals, identifiers, parens, lists, maps).
    Value parseAtom()
    {
        auto tok = peek();

        switch (tok.kind)
        {
        case Token.Kind.intLit:
            advance();
            return parseIntLit(tok);

        case Token.Kind.uintLit:
            advance();
            return parseUintLit(tok);

        case Token.Kind.floatLit:
            advance();
            return parseFloatLit(tok);

        case Token.Kind.stringLit:
            advance();
            return Value(parseStringContent(tok.text));

        case Token.Kind.bytesLit:
            advance();
            return Value(cast(immutable(ubyte)[]) parseStringContent(tok.text));

        case Token.Kind.trueKw:
            advance();
            return Value(true);

        case Token.Kind.falseKw:
            advance();
            return Value(false);

        case Token.Kind.nullKw:
            advance();
            return Value.null_();

        case Token.Kind.ident:
            advance();
            // Function call: ident(args...)
            if (peek().kind == Token.Kind.lparen)
            {
                advance();
                auto args = parseArgList();
                expect(Token.Kind.rparen);
                return evalFunction(tok.text, args, tok.pos);
            }
            // Variable lookup
            return ctx(tok.text);

        case Token.Kind.lparen:
            advance();
            auto inner = parseExpr(0);
            expect(Token.Kind.rparen);
            return inner;

        case Token.Kind.lbracket:
            advance();
            return parseList();

        case Token.Kind.lbrace:
            advance();
            return parseMap();

        default:
            throw new EvalException("unexpected " ~ tok.toString(), tok.pos);
        }
    }

    /// Parse a ternary conditional (after consuming '?').
    Value parseTernary(Value cond)
    {
        auto thenVal = parseExpr(0);
        expect(Token.Kind.colon);
        auto elseVal = parseExpr(0);
        // Error in condition propagates
        if (cond.type == Value.Type.err)
            return cond;
        return isTruthy(cond) ? thenVal : elseVal;
    }

    /// Parse a comma-separated argument list (without parens).
    Value[] parseArgList()
    {
        Value[] args;
        if (peek().kind == Token.Kind.rparen)
            return args;
        args ~= parseExpr(0);
        while (match(Token.Kind.comma))
            args ~= parseExpr(0);
        return args;
    }

    /// Parse a list literal [...].
    Value parseList()
    {
        Value[] elems;
        if (peek().kind == Token.Kind.rbracket)
        {
            advance();
            return Value(elems);
        }
        elems ~= parseExpr(0);
        while (match(Token.Kind.comma))
        {
            if (peek().kind == Token.Kind.rbracket)
                break; // trailing comma
            elems ~= parseExpr(0);
        }
        expect(Token.Kind.rbracket);
        return Value(elems);
    }

    /// Parse a map literal {...}.
    Value parseMap()
    {
        Value[string] entries;
        if (peek().kind == Token.Kind.rbrace)
        {
            advance();
            return Value(entries);
        }

        parseMapEntry(entries);
        while (match(Token.Kind.comma))
        {
            if (peek().kind == Token.Kind.rbrace)
                break; // trailing comma
            parseMapEntry(entries);
        }
        expect(Token.Kind.rbrace);
        return Value(entries);
    }

    void parseMapEntry(ref Value[string] entries)
    {
        auto keyTok = peek();
        auto keyVal = parseExpr(0);
        expect(Token.Kind.colon);
        auto valExpr = parseExpr(0);

        // Extract string key — use match directly to avoid const issues
        string keyStr = keyVal.inner.match!((ref string s) => s, (ref _) => null,);
        if (keyStr is null)
            throw new EvalException("map keys must be strings", keyTok.pos);
        entries[keyStr] = valExpr;
    }
}

// ── Operator precedence ─────────────────────────────────────────────

/// Returns the precedence for an infix operator, or -1 if not infix.
private int infixPrec(Token.Kind kind)
{
    switch (kind)
    {
    case Token.Kind.question:
        return 1; // ternary
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
        return 8; // member access, indexing
    default:
        return -1;
    }
}

// ── Evaluation helpers ──────────────────────────────────────────────

/// Check if a Value is an error, propagating it if so.
private bool isErr(Value v)
{
    return v.type == Value.Type.err;
}

private Value evalBinary(Token.Kind op, Value lhs, Value rhs)
{
    // Propagate errors
    if (isErr(lhs))
        return lhs;
    if (isErr(rhs))
        return rhs;

    // Arithmetic: int × int
    auto li = tryGet!long(lhs);
    auto ri = tryGet!long(rhs);
    if (!li.isNull && !ri.isNull)
    {
        switch (op)
        {
        case Token.Kind.plus:
            return Value(li.get + ri.get);
        case Token.Kind.minus:
            return Value(li.get - ri.get);
        case Token.Kind.star:
            return Value(li.get * ri.get);
        case Token.Kind.slash:
            if (ri.get == 0)
                return Value.err("division by zero");
            return Value(li.get / ri.get);
        case Token.Kind.percent:
            if (ri.get == 0)
                return Value.err("modulo by zero");
            return Value(li.get % ri.get);
        case Token.Kind.eqEq:
            return Value(li.get == ri.get);
        case Token.Kind.bangEq:
            return Value(li.get != ri.get);
        case Token.Kind.lt:
            return Value(li.get < ri.get);
        case Token.Kind.ltEq:
            return Value(li.get <= ri.get);
        case Token.Kind.gt:
            return Value(li.get > ri.get);
        case Token.Kind.gtEq:
            return Value(li.get >= ri.get);
        default:
            break;
        }
    }

    // Arithmetic: uint × uint
    auto lu = tryGet!ulong(lhs);
    auto ru = tryGet!ulong(rhs);
    if (!lu.isNull && !ru.isNull)
    {
        switch (op)
        {
        case Token.Kind.plus:
            return Value(lu.get + ru.get);
        case Token.Kind.minus:
            return Value(lu.get - ru.get);
        case Token.Kind.star:
            return Value(lu.get * ru.get);
        case Token.Kind.slash:
            if (ru.get == 0)
                return Value.err("division by zero");
            return Value(lu.get / ru.get);
        case Token.Kind.percent:
            if (ru.get == 0)
                return Value.err("modulo by zero");
            return Value(lu.get % ru.get);
        case Token.Kind.eqEq:
            return Value(lu.get == ru.get);
        case Token.Kind.bangEq:
            return Value(lu.get != ru.get);
        case Token.Kind.lt:
            return Value(lu.get < ru.get);
        case Token.Kind.ltEq:
            return Value(lu.get <= ru.get);
        case Token.Kind.gt:
            return Value(lu.get > ru.get);
        case Token.Kind.gtEq:
            return Value(lu.get >= ru.get);
        default:
            break;
        }
    }

    // Arithmetic: double × double (or mixed int/double promotion)
    auto ld = toDouble(lhs);
    auto rd = toDouble(rhs);
    if (!ld.isNull && !rd.isNull)
    {
        switch (op)
        {
        case Token.Kind.plus:
            return Value(ld.get + rd.get);
        case Token.Kind.minus:
            return Value(ld.get - rd.get);
        case Token.Kind.star:
            return Value(ld.get * rd.get);
        case Token.Kind.slash:
            return Value(ld.get / rd.get);
        case Token.Kind.percent:
            return Value(ld.get % rd.get);
        case Token.Kind.eqEq:
            return Value(ld.get == rd.get);
        case Token.Kind.bangEq:
            return Value(ld.get != rd.get);
        case Token.Kind.lt:
            return Value(ld.get < rd.get);
        case Token.Kind.ltEq:
            return Value(ld.get <= rd.get);
        case Token.Kind.gt:
            return Value(ld.get > rd.get);
        case Token.Kind.gtEq:
            return Value(ld.get >= rd.get);
        default:
            break;
        }
    }

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

    // String contains
    auto ls = tryGet!string(lhs);
    auto rs = tryGet!string(rhs);
    if (!ls.isNull && !rs.isNull)
    {
        import std.algorithm : canFind;

        return Value(rs.get.canFind(ls.get));
    }

    // List membership
    const rl = tryGet!(Value[])(rhs);
    if (!rl.isNull)
    {
        // TODO: deep equality check
        return Value(false);
    }

    // Map key membership
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

    // Map field access
    const m = tryGet!(Value[string])(obj);
    if (!m.isNull)
    {
        if (auto p = name in m.get)
            return *p;
        return Value.err("no such key: " ~ name);
    }
    // Entry (lazy) field access
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

    // List indexing
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

    // Map indexing
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
    // Built-in functions
    switch (name)
    {
    case "size":
        if (args.length != 1)
            throw new EvalException("size() takes exactly 1 argument", pos);
        return evalSize(args[0]);
    case "has":
        // TODO: has() requires macro-like behavior (don't eval the arg).
        throw new EvalException("has() is not yet implemented", pos);
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
        // TODO: regex matching
        return Value.err(".matches() is not yet implemented");
    default:
        return Value.err("unknown method: " ~ name);
    }
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
    return Value.err("cannot convert " ~ typeName(v) ~ " to string");
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

/// Parse the content of a string literal (strip quotes, process escapes).
private string parseStringContent(string raw)
{
    // Determine prefix length (b, r, br, etc.)
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

    // Triple-quoted?
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
                // Unknown escape: keep as-is
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

// ── Value helpers ───────────────────────────────────────────────────

import std.typecons : Nullable, nullable;

private Nullable!T tryGet(T)(Value v)
{
    return v.inner.match!((ref T val) => nullable(val), (ref _) => Nullable!T.init,);
}

/// Try to promote a numeric Value to double.
private Nullable!double toDouble(Value v)
{
    return v.inner.match!((ref double d) => nullable(d),
            (ref long i) => nullable(cast(double) i),
            (ref ulong u) => nullable(cast(double) u), (ref _) => Nullable!(double).init,);
}

private bool isTruthy(Value v)
{
    return v.inner.match!((ref bool b) => b, (ref _) => true, // non-bool values are truthy in CEL
            );
}

private bool isFalsy(Value v)
{
    return !isTruthy(v);
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
    case Value.Type.err:
        return "error";
    }
}

private string kindName(Token.Kind kind)
{
    import std.conv : to;

    return kind.to!string;
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

    // Division by zero is now an error value, not an exception
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

    // emptyContext returns err values, which are still Values (not exceptions)
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

    // false && (error) should produce false, not propagate error
    evaluate("false && (1/0 == 1)", emptyContext()).should.be(value(false));
    // true || (error) should produce true, not propagate error
    evaluate("true || (1/0 == 1)", emptyContext()).should.be(value(true));
}

@("Eval: error propagation")
unittest
{
    import dshould;

    // Errors propagate through operators
    evaluate("1/0 + 1", emptyContext()).type.should.be(Value.Type.err);
    evaluate("-(1/0)", emptyContext()).type.should.be(Value.Type.err);
    // But short-circuit absorbs errors
    evaluate("true || (1/0 == 1)", emptyContext()).should.be(value(true));
    evaluate("false && (1/0 == 1)", emptyContext()).should.be(value(false));
    // Non-short-circuit propagates
    evaluate("true && (1/0 == 1)", emptyContext()).type.should.be(Value.Type.err);
    evaluate("false || (1/0 == 1)", emptyContext()).type.should.be(Value.Type.err);
}
