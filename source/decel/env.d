/++
 + CEL Environment — global evaluation configuration.
 +
 + Holds macro and method-macro registrations. Constructed once, immutable
 + during evaluation. This is the extension point for custom functions.
 +/
module decel.env;

import decel.context;
import decel.lexer;
import decel.value;

/// A function-call macro. Invoked when the parser sees `name(...)`.
/// The opening '(' has already been consumed; the macro must parse its
/// arguments and consume the closing ')' before returning.
alias Macro = Value delegate(ref TokenRange r, const Env env, Context ctx);

/// A method-call macro. Invoked when the parser sees `expr.name(...)`.
/// The opening '(' has already been consumed; the macro must parse its
/// arguments and consume the closing ')' before returning.
/// `target` is the already-evaluated receiver expression.
alias MethodMacro = Value delegate(Value target, ref TokenRange r, const Env env, Context ctx);

/++
 + Exception thrown for parse errors (syntax errors, unexpected tokens).
 + Evaluation errors (division by zero, type mismatches) are represented
 + as Value.err instead.
 +/
class EvalException : Exception
{
    /// Byte offset in the source where the error occurred.
    immutable size_t position;

    /// Construct an EvalException with a message and source position.
    this(string msg, size_t pos, string file = __FILE__, size_t line = __LINE__)
    {
        import std.format : format;

        super(format!"at position %d: %s"(pos, msg), file, line);
        position = pos;
    }
}

/// Human-readable name for a token kind.
string kindName(Token.Kind kind)
{
    import std.conv : to;

    return kind.to!string;
}

/// Token cursor — the linear parser state. Just a position in a token array.
struct TokenRange
{
    /// The full token array (shared, never modified).
    Token[] tokens;
    /// Current position in the token array.
    size_t pos;

    /// Current token.
    Token peek() const
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
        {
            throw new EvalException("expected " ~ kindName(kind) ~ ", got " ~ tok.toString(),
                    tok.pos);
        }
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

    /// Snapshot current position for replay (e.g. comprehension bodies).
    TokenRange save()
    {
        return TokenRange(tokens, pos);
    }
}

/// Evaluation environment — global configuration for the evaluator.
struct Env
{
    /// Registered macros (function-call style).
    Macro[string] macros;
    /// Registered method macros (method-call style).
    MethodMacro[string] methodMacros;
}
