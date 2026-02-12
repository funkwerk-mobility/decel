/++
 + CEL Program — a compiled expression ready for evaluation.
 +/
module decel2.program;

import decel2.context;
import decel2.eval;
import decel2.value;

/// A compiled CEL program. Stores the source and evaluates on demand.
struct Program
{
    /// The original source expression.
    string source;

    /// Evaluate this program against the given context.
    Value eval(Context ctx)
    {
        return evaluate(source, ctx);
    }
}

@("Program: eval returns correct result")
unittest
{
    import dshould;

    auto prog = Program("1 + 2");
    prog.eval(emptyContext()).should.be(value(3L));
}

@("Program: eval with context")
unittest
{
    import dshould;

    auto prog = Program("x + 1");
    auto ctx = contextFrom(["x": value(10L)]);
    prog.eval(ctx).should.be(value(11L));
}
