/++
 + decel - A Google Common Expression Language (CEL) implementation for D.
 +
 + CEL is a non-Turing-complete expression language designed for fast, portable,
 + and safe evaluation of simple expressions.
 +
 + The public API consists of:
 + $(UL
 +   $(LI `evaluate` / `evaluateWithMacros` — evaluate a CEL expression string)
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
public import decel.value : Value, Entry, value;

// Context (variable bindings)
public import decel.context : Context, contextFrom, emptyContext, bindContext;

// Evaluation
public import decel.eval : evaluate, evaluateWithMacros, EvalException;

// Extension points
public import decel.env : Env, Macro, MethodMacro;
