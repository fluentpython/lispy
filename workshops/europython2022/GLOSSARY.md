# GLOSSARY

Programming language terms used in this workshop:

**AST**: Abstract Syntax Tree; a data structure that represents the program in
a way that is suitable for processing by an _interpreter_ or _compiler_.
Python's lists, dictionaries, and/or nested objects are useful to build an AST.
Python's own internal AST is built from nested objects using classes defined in
the [`ast` module](https://docs.python.org/3/library/ast.html) of the standard library.

**compiler**: a program which reads source code and produces binary code that
can be executed by a CPU or a virtual machine.
CPython includes a compiler that generates Python bytecode which is then
executed by a bytecode _interpreter_ at runtime.
This is similar to the way Java works,
but most Python users don't need to know about the compiler because
it runs automatically, generating bytecode in memory or on disk—for imported modules.

**host language**: the language in which an interpreter or compiler is written,
as oposed to the language processed by the interpreter or compiler.
The host language of `lis.py` is Python, and the host language of CPython is C.

**infix notation**: expression syntax with operators used between operands: `a + b`.
Contrast with _prefix notation_.

**interpreter**: a program that executes code in memory,
without generating a binary executable.
Simple interpreters like `lis.py` read source code, produce an AST,
and execute the user program by traversing the AST directly.
Python's "interpreter" automatically compiles the AST to bytecode,
and its runtime environment interprets the bytecode.
See _compiler_.

**parser**: function which transforms source code into an _AST_,
built with objects in the _host language_.
For example, the `lis.py` function transforms `(* 2 pi)` into `['*', 2, 'pi']`.

**prefix notation**: expression syntax with operators preceding operands.
For example `+ 1 2 3 4` is the sum of those four numbers.
In Scheme, prefix expressions are delimited by parenthesis: `(+ a b c d)`
Contrast with _infix notation_

**prompt**: a symbol displayed by an interactive interpreter to indicate it
is ready to accept input. In Python, the standard prompt is `>>>`;
in the Bash shell, the default is `$`.

**REPL**: Read-Eval-Print-Loop; an interactive program which reads user input,
evaluates (i.e. executes), prints a result, and loops back to accept more
input, often displaying a _prompt_.

**variadic**: a function parameter that accepts zero or more arguments.
For example: `def add(*p): return sum(p)` creates a Python function 
accepting one or more arguments that are collected into the `p` parameter,
so we can call it as `add()`, `add(3, 4)`, `add(1, 2, 3 4)`, or even
`add(*range(1_000_000))`.
In every case, the `p` parameter in `add` gets a tuple of zero or more values.
The prefix notation of Scheme supports variadic operators,
so `(+ 1 2 3 4)` evaluates to `10`.

