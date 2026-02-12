/++
 + CEL Environment — compiles CEL source into executable programs.
 +/
module decel.env;

import decel.program;

/// The CEL environment: holds type declarations, function bindings,
/// and compiles CEL expressions into Programs.
struct Env
{
    /// Create a standard CEL environment with all built-in functions.
    static Env standard()
    {
        return Env.init; // TODO: register standard functions
    }

    /// Compile a CEL expression string into a Program.
    /// Returns a Program that can be evaluated against a Context.
    Program compile(string expression)
    {
        return Program(expression); // TODO: parse, type-check, produce AST
    }
}

@("Env: compile returns a Program")
unittest
{
    import dshould;
    import decel.value;
    import decel.context;

    auto env = Env.standard();
    auto prog = env.compile("1 + 1");
    auto result = prog.eval(emptyContext());
    // TODO: once eval is implemented, check result
    result.type.should.be(Value.Type.err); // stub returns error for now
}
