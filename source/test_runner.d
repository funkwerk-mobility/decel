module test_runner;

import unit_threaded;

mixin runTestsMain!("decel", "decel.value", "decel.context", "decel.eval",
        "decel.lexer", "decel.program",);
