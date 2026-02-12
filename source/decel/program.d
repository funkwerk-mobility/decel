/++
 + CEL Program — a compiled expression ready for evaluation.
 +/
module decel.program;

import decel.context;
import decel.value;

/// A compiled CEL program. Produced by Env.compile().
/// Evaluate against a Context to get a result Value.
struct Program
{
    /// The original source expression. TODO: replace with AST.
    string source;

    /// Evaluate this program against the given context.
    Value eval(Context ctx)
    {
        // TODO: walk AST, evaluate @nogc using ctx
        cast(void) ctx;
        return Value.err("not implemented");
    }
}

@("Program: eval stub returns error")
unittest
{
    import dshould;

    auto prog = Program("true");
    auto result = prog.eval(emptyContext());
    result.type.should.be(Value.Type.err);
}
