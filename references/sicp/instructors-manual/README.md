# Concrete Metacircular Evaluator

Code adapted from the "evaluator with concrete syntax" listed on pages 121-122 of
["Instructor's Manual to accompany Structure and Interpretation of Computer Programs"](https://mitpress.mit.edu/books/instructors-manual-ta-structure-and-interpretation-computer-programs-second-edition)
by Julie Sussman with Harold Abelson and Gerald Jay Sussman.

Tested with [Racket](https://racket-lang.org/) 8.3.

Changes from the original code:

* function names changed to ALL-CAPS to avoid confusion with
  functions provided by Racket;
* parameters named `env` renamed to `environ` to avoid confusion with `exp`;
* use of `#t`, `#f` instead of `true`, `false`;
* APPLY rewritten to avoid the need for `primitive-procedure?`;
  the corresponding function in Racket is `primitive?` but it is not
  useful to us because `(primitive? +)` returns `#f`, so we can't use it
  to distinguish user-defined procedures from those predefined by Racket;
* added MAKE-BUILTINS to create an environment with predefined procedures;
* added `concrete-test.rkt` with `rackunit` tests;
* added `(provide ...)` to make `concrete.rkt` functions available for testing.
