/++
 + decel - A Google Common Expression Language (CEL) implementation for D.
 +
 + CEL is a non-Turing-complete expression language designed for fast, portable,
 + and safe evaluation of simple expressions.
 +
 + The public API consists of:
 + $(UL
 +   $(LI `evaluate` / `evaluateWithMacros` — evaluate a CEL expression string)
 +   $(LI `now()` — built-in function returning the current timestamp)
 +   $(LI `Value` / `value` — the runtime value type and convenience constructor)
 +   $(LI `Context` / `contextFrom` / `emptyContext` / `bindContext` — variable bindings)
 +   $(LI `Macro` / `MethodMacro` / `Env` — extension points for custom functions)
 +   $(LI `EvalException` — thrown on syntax errors)
 + )
 +
 + See_Also: https://cel.dev/
 +/
module decel;

// Value types
public import decel.value : Value, Entry, List, ArrayList, value;

// Context (variable bindings)
public import decel.context : Context, contextFrom, emptyContext, bindContext;

// Evaluation
public import decel.eval : evaluate, evaluateWithMacros;

// Exception type (re-exported via eval and env too, but canonical source is here)
public import decel.exception : EvalException;

// Extension points
public import decel.env : Env, Macro, MethodMacro, TokenRange;

// Lexer (needed for custom macros)
public import decel.lexer : Token;

// Parser (needed for custom macros that parse arguments)
public import decel.eval : parseExpr, parseArgList;
