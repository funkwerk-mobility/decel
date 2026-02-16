/++
 + CEL Lexer — tokenizes CEL expression source into a token stream.
 +/
module decel.lexer;

/// A single token produced by the lexer.
struct Token
{
    /// The kind of token.
    Kind kind;
    /// The source text of this token.
    string text;
    /// Byte offset in the source where this token starts.
    size_t pos;

    /// All possible token kinds.
    enum Kind
    {
        // Literals
        intLit,
        uintLit,
        floatLit,
        stringLit,
        bytesLit,

        // Identifiers and keywords
        ident,
        trueKw,
        falseKw,
        nullKw,
        inKw,

        // Operators
        plus,
        minus,
        star,
        slash,
        percent,
        bang,
        eqEq,
        bangEq,
        lt,
        ltEq,
        gt,
        gtEq,
        ampAmp,
        pipePipe,
        question,
        colon,

        // Punctuation
        lparen,
        rparen,
        lbracket,
        rbracket,
        lbrace,
        rbrace,
        dot,
        comma,

        // Special
        eof,
        err,
    }

    /// Human-readable description for error messages.
    string toString() const
    {
        import std.format : format;

        if (kind == Kind.eof)
            return "<eof>";
        if (kind == Kind.err)
            return format!"<error: %s>"(text);
        return text;
    }
}

/// Tokenize a CEL expression string into a token array.
Token[] tokenize(string source)
{
    Token[] tokens;
    size_t i = 0;

    while (i < source.length)
    {
        if (source[i] == ' ' || source[i] == '\t' || source[i] == '\n' || source[i] == '\r')
        {
            i++;
            continue;
        }

        if (i + 1 < source.length && source[i] == '/' && source[i + 1] == '/')
        {
            while (i < source.length && source[i] != '\n')
                i++;
            continue;
        }

        auto start = i;

        if (isIdentStart(source[i]))
        {
            while (i < source.length && isIdentCont(source[i]))
                i++;
            auto word = source[start .. i];

            if ((word == "b" || word == "B" || word == "r" || word == "R"
                    || word == "br" || word == "bR" || word == "Br" || word == "BR"
                    || word == "rb" || word == "rB" || word == "Rb" || word == "RB")
                    && i < source.length && (source[i] == '"' || source[i] == '\''))
            {
                bool isBytes = false;
                bool isRaw = false;
                foreach (c; word)
                {
                    if (c == 'b' || c == 'B')
                        isBytes = true;
                    if (c == 'r' || c == 'R')
                        isRaw = true;
                }
                auto tok = lexString(source, i, start, isRaw);
                tok.kind = isBytes ? Token.Kind.bytesLit : Token.Kind.stringLit;
                tokens ~= tok;
                i = start + tok.text.length;
                continue;
            }

            auto kind = identKind(word);
            tokens ~= Token(kind, word, start);
            continue;
        }

        if (isDigit(source[i]) || (source[i] == '.' && i + 1 < source.length
                && isDigit(source[i + 1])))
        {
            tokens ~= lexNumber(source, i, start);
            continue;
        }

        if (source[i] == '"' || source[i] == '\'')
        {
            auto tok = lexString(source, i, start, false);
            tokens ~= tok;
            i = start + tok.text.length;
            continue;
        }

        if (i + 1 < source.length)
        {
            auto two = source[i .. i + 2];
            Token.Kind twoKind;
            bool foundTwo = true;
            switch (two)
            {
            case "==":
                twoKind = Token.Kind.eqEq;
                break;
            case "!=":
                twoKind = Token.Kind.bangEq;
                break;
            case "<=":
                twoKind = Token.Kind.ltEq;
                break;
            case ">=":
                twoKind = Token.Kind.gtEq;
                break;
            case "&&":
                twoKind = Token.Kind.ampAmp;
                break;
            case "||":
                twoKind = Token.Kind.pipePipe;
                break;
            default:
                foundTwo = false;
                break;
            }
            if (foundTwo)
            {
                tokens ~= Token(twoKind, two, start);
                i += 2;
                continue;
            }
        }

        Token.Kind singleKind;
        bool foundSingle = true;
        switch (source[i])
        {
        case '+':
            singleKind = Token.Kind.plus;
            break;
        case '-':
            singleKind = Token.Kind.minus;
            break;
        case '*':
            singleKind = Token.Kind.star;
            break;
        case '/':
            singleKind = Token.Kind.slash;
            break;
        case '%':
            singleKind = Token.Kind.percent;
            break;
        case '!':
            singleKind = Token.Kind.bang;
            break;
        case '<':
            singleKind = Token.Kind.lt;
            break;
        case '>':
            singleKind = Token.Kind.gt;
            break;
        case '?':
            singleKind = Token.Kind.question;
            break;
        case ':':
            singleKind = Token.Kind.colon;
            break;
        case '(':
            singleKind = Token.Kind.lparen;
            break;
        case ')':
            singleKind = Token.Kind.rparen;
            break;
        case '[':
            singleKind = Token.Kind.lbracket;
            break;
        case ']':
            singleKind = Token.Kind.rbracket;
            break;
        case '{':
            singleKind = Token.Kind.lbrace;
            break;
        case '}':
            singleKind = Token.Kind.rbrace;
            break;
        case '.':
            singleKind = Token.Kind.dot;
            break;
        case ',':
            singleKind = Token.Kind.comma;
            break;
        default:
            foundSingle = false;
            break;
        }
        if (foundSingle)
        {
            tokens ~= Token(singleKind, source[i .. i + 1], start);
            i++;
            continue;
        }

        tokens ~= Token(Token.Kind.err, source[i .. i + 1], start);
        i++;
    }

    tokens ~= Token(Token.Kind.eof, "", source.length);
    return tokens;
}

private bool isIdentStart(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

private bool isIdentCont(char c)
{
    return isIdentStart(c) || isDigit(c);
}

private bool isDigit(char c)
{
    return c >= '0' && c <= '9';
}

private bool isHexDigit(char c)
{
    return isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

private Token.Kind identKind(string word)
{
    switch (word)
    {
    case "true":
        return Token.Kind.trueKw;
    case "false":
        return Token.Kind.falseKw;
    case "null":
        return Token.Kind.nullKw;
    case "in":
        return Token.Kind.inKw;
    default:
        return Token.Kind.ident;
    }
}

private Token lexNumber(string source, ref size_t i, size_t start)
{
    bool isFloat = false;

    if (i + 1 < source.length && source[i] == '0' && (source[i + 1] == 'x' || source[i + 1] == 'X'))
    {
        i += 2;
        if (i >= source.length || !isHexDigit(source[i]))
            return Token(Token.Kind.err, source[start .. i], start);
        while (i < source.length && isHexDigit(source[i]))
            i++;
        if (i < source.length && (source[i] == 'u' || source[i] == 'U'))
        {
            i++;
            return Token(Token.Kind.uintLit, source[start .. i], start);
        }
        return Token(Token.Kind.intLit, source[start .. i], start);
    }

    while (i < source.length && isDigit(source[i]))
        i++;

    if (i < source.length && source[i] == '.' && (i + 1 >= source.length || source[i + 1] != '.'))
    {
        isFloat = true;
        i++;
        while (i < source.length && isDigit(source[i]))
            i++;
    }

    if (i < source.length && (source[i] == 'e' || source[i] == 'E'))
    {
        isFloat = true;
        i++;
        if (i < source.length && (source[i] == '+' || source[i] == '-'))
            i++;
        if (i >= source.length || !isDigit(source[i]))
            return Token(Token.Kind.err, source[start .. i], start);
        while (i < source.length && isDigit(source[i]))
            i++;
    }

    if (isFloat)
        return Token(Token.Kind.floatLit, source[start .. i], start);

    if (i < source.length && (source[i] == 'u' || source[i] == 'U'))
    {
        i++;
        return Token(Token.Kind.uintLit, source[start .. i], start);
    }

    return Token(Token.Kind.intLit, source[start .. i], start);
}

private Token lexString(string source, ref size_t i, size_t start, bool isRaw)
{
    const quote = source[i];
    bool triple = false;

    if (i + 2 < source.length && source[i + 1] == quote && source[i + 2] == quote)
    {
        triple = true;
        i += 3;
    }
    else
    {
        i++;
    }

    while (i < source.length)
    {
        if (source[i] == '\\' && !isRaw)
        {
            i += 2;
            continue;
        }
        if (source[i] == quote)
        {
            if (triple)
            {
                if (i + 2 < source.length && source[i + 1] == quote && source[i + 2] == quote)
                {
                    i += 3;
                    return Token(Token.Kind.stringLit, source[start .. i], start);
                }
                i++;
                continue;
            }
            else
            {
                i++;
                return Token(Token.Kind.stringLit, source[start .. i], start);
            }
        }
        if (!triple && (source[i] == '\n' || source[i] == '\r'))
        {
            return Token(Token.Kind.err, source[start .. i], start);
        }
        i++;
    }

    return Token(Token.Kind.err, source[start .. i], start);
}

@("Lexer: simple arithmetic")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize("1 + 2 * 3");
    with (Token.Kind)
        tokens.map!"a.kind".should.be([intLit, plus, intLit, star, intLit, eof]);
    tokens.map!"a.text".should.be(["1", "+", "2", "*", "3", ""]);
}

@("Lexer: identifiers and keywords")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize("x in true false null foo_bar");
    with (Token.Kind)
        tokens.map!"a.kind".should.be([
        ident, inKw, trueKw, falseKw, nullKw, ident, eof
    ]);
    tokens.map!"a.text".should.be([
        "x", "in", "true", "false", "null", "foo_bar", ""
    ]);
}

@("Lexer: comparison and logical operators")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize("a == b && c != d || e <= f >= g");
    with (Token.Kind)
        tokens.map!"a.kind".should.be([
        ident, eqEq, ident, ampAmp, ident, bangEq, ident, pipePipe, ident,
        ltEq, ident, gtEq, ident, eof
    ]);
}

@("Lexer: string literals")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize(`"hello" 'world'`);
    with (Token.Kind)
        tokens.map!"a.kind".should.be([stringLit, stringLit, eof]);
    tokens.map!"a.text".should.be([`"hello"`, `'world'`, ""]);
}

@("Lexer: numeric literals")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize("42 3u 0xFF 0x1Au 3.14 1e10 2.5e-3");
    with (Token.Kind)
        tokens.map!"a.kind".should.be([
        intLit, uintLit, intLit, uintLit, floatLit, floatLit, floatLit, eof
    ]);
    tokens.map!"a.text".should.be([
        "42", "3u", "0xFF", "0x1Au", "3.14", "1e10", "2.5e-3", ""
    ]);
}

@("Lexer: punctuation and grouping")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize("foo(a, b[0]).bar ? x : y");
    with (Token.Kind)
        tokens.map!"a.kind".should.be([
        ident, lparen, ident, comma, ident, lbracket, intLit, rbracket, rparen,
        dot, ident, question, ident, colon, ident, eof
    ]);
}

@("Lexer: escape sequences in strings")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize(`"hello\nworld" "tab\there"`);
    with (Token.Kind)
        tokens.map!"a.kind".should.be([stringLit, stringLit, eof]);
}

@("Lexer: empty input")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize("");
    with (Token.Kind)
        tokens.map!"a.kind".should.be([eof]);
}

@("Lexer: unary negation and subtraction")
unittest
{
    import std.algorithm : map;
    import dshould;

    auto tokens = tokenize("-1 + a - b");
    with (Token.Kind)
        tokens.map!"a.kind".should.be([
        minus, intLit, plus, ident, minus, ident, eof
    ]);
}
