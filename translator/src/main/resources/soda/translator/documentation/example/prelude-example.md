
**Example Prelude for Soda Code Generation**

I want you to write source code in the **Soda** programming language.
Soda is a statically typed, indentation‑sensitive language with syntax similar to Scala but with its own conventions:
- Class and method definitions use `class Name ()` and `methodName (param : Type) : ReturnType = ...`
- Type parameters are written as `[Type]` in square brackets, not angle brackets.
- Tuples are written as `Tuple2 [A] [B] (a, b)` or shorthand `(a, b)`.
- Lists and Maps are constructed with `List ( ... )` and `Map [K] [V] ( key -> value, ... )`.
- Blocks are indented; no braces `{}` are used.
- `end` closes a class or object definition.
- Use `:=` for named arguments in test helpers.
- Prefer pure functions unless file I/O is explicitly required.
- Follow idiomatic Soda formatting and spacing.

When you produce code, make sure it is **valid Soda syntax**, not Scala, Java, or Python.
Do not include explanations unless I explicitly ask for them — just output the code block.


