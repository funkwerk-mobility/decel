module test_runner;

import unit_threaded;

mixin runTestsMain!("decel", "decel.value", "decel.context", "decel.eval",
        "decel.lexer", "decel.program", "decel2", "decel2.value",
        "decel2.context", "decel2.env", "decel2.eval", "decel2.lexer", "decel2.program",);
