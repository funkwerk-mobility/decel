/++
 + CEL Environment — global evaluation configuration.
 +
 + Holds macro and method-macro registrations. Constructed once, immutable
 + during evaluation. This is the extension point for custom functions.
 +/
module decel2.env;

import decel2.context;
import decel2.lexer;
import decel2.value;

/// Forward reference to TokenRange (defined in eval module).
/// We use Token[] + size_t here to avoid circular imports.

/// A macro receives the token range, env, and context to handle its own
/// argument parsing. The opening '(' has already been consumed; the macro
/// must consume ')'.
alias Macro = Value delegate(ref TokenRange r, const Env env, Context ctx);

/// A method macro receives the target object, token range, env, and context.
/// The opening '(' has already been consumed; the macro must consume ')'.
alias MethodMacro = Value delegate(Value target, ref TokenRange r, const Env env, Context ctx);

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
            import decel2.eval : EvalException, kindName;

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

    /// Create a standard environment with all built-in macros.
    static Env standard()
    {
        import decel2.eval : builtinMacros, builtinMethodMacros;

        Env e;
        e.macros = builtinMacros();
        e.methodMacros = builtinMethodMacros();
        return e;
    }

    /// Create a standard environment merged with user-supplied macros.
    static Env withMacros(Macro[string] userMacros)
    {
        auto e = standard();
        if (userMacros !is null)
        {
            foreach (k, v; userMacros)
                e.macros[k] = v;
        }
        return e;
    }
}
