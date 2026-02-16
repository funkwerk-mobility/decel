/++
 + CEL Exception type — shared across the decel library.
 +/
module decel.exception;

/++
 + Exception thrown for parse errors (syntax errors, unexpected tokens)
 + and type-mismatch errors when extracting values via `Value.get!T`.
 +
 + Evaluation errors (division by zero, missing keys) are represented
 + as `Value.err` instead — only structural/API misuse throws.
 +/
class EvalException : Exception
{
    /// Byte offset in the source where the error occurred (0 for non-parse errors).
    immutable size_t position;

    /// Construct an EvalException with a message and source position.
    this(string msg, size_t pos, string file = __FILE__, size_t line = __LINE__)
    {
        import std.format : format;

        super(format!"at position %d: %s"(pos, msg), file, line);
        position = pos;
    }

    /// Construct an EvalException with just a message (no source position).
    this(string msg, string file = __FILE__, size_t line = __LINE__)
    {
        super(msg, file, line);
        position = 0;
    }
}
