/++
 + decel - A Google Common Expression Language (CEL) implementation for D.
 +
 + CEL is a non-Turing-complete expression language designed for fast, portable,
 + and safe evaluation of simple expressions.
 +
 + See_Also: https://cel.dev/
 +/
module decel;

public import decel.value;
public import decel.context;
public import decel.env;
public import decel.eval;
public import decel.lexer;
public import decel.program;
