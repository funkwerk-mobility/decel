module test_runner;

import unit_threaded;

mixin runTestsMain!("decel", "decel.value", "decel.context", "decel.env", "decel.program",);
