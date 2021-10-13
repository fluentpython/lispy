# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.13.0
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# # O `lis.py` de Norvig
#
#
# ![Norvig's lispy](lispy.png)

# Contents:
#
# * [Introdução](#Introdução)
# * [Sintaxe de Scheme](#Sintaxe-de-Scheme)
# * [Imports and types](#Imports-and-types)
# * [The Parser](#The-Parser)
#   * [Exercise 0](#Exercise-0)
# * [The Built-in Environment](#The-Built-in-Environment)
# * [A Calculator](#A-Calculator)
# * [Non-interactive execution](#Non-interactive-execution)
#   * [Exercise 1](#Exercise-1)
#   * [Exercise 2](#Exercise-2)
# * [User defined procedures](#User-defined-procedures)
# * [A more complete environment](#A-more-complete-environment)
# * [`Procedure`: a class to represent a closure](#Procedure:-a-class-to-represent-a-closure)
# * [Evaluate with `lambda`, `if`, and `quote`](#Evaluate-with-lambda,-if,-and-quote)
# * [The REPL](#The-REPL)
# * [Examples](#Examples)
# * [Syntactic sugar](#Syntactic-sugar)
#
# > **LICENSES**:<br>
#   Code © 2010-2018 Peter Norvig, [MIT License](https://github.com/fluentpython/lispy/blob/main/LICENSE)<br>
#   Text © 2022 Luciano Ramalho, [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/)
#

# ## Introdução
#
# [Peter Norvig](https://norvig.com/) da universidade Stanford criou
# [`lis.py`](https://github.com/norvig/pytudes/blob/main/py/lis.py):
# um interpretador em 132 linhas de código Python legível,
# para parte da linguagem Scheme—um dialeto de Lisp.
#
# Porque você deveria estudar `lis.py`?
# Para mim esses foram alguns motivos:
#
# * Depeois de aprender como funciona um interpretador,
# passei a entender com mais profundidade como funciona a Python e linguagens em geral—interpretadas ou compiladas.
#
# * A simplicidade de Scheme é uma aula magna de design de linguagens;
#
# * `lis.py` é um lindo exemplo de código Python idiomático.
#
# Norvig descreve `lis.py` em um texto intitulado. 
# [(How to Write a (Lisp) Interpreter (in Python))](https://norvig.com/lispy.html). Altamente recomendado.
#
# Antes de examinar o código do interpretador em Python, vamos ver um pouco de Scheme—caso você nunca tenha visto essa linguagem ou Lisp anteriormente.

# ## Sintaxe de Scheme
#
# Todo código Scheme é formado por expressões.
# Não existem operadores infixos:
# todas as expressões usam notação prefixa como 
# `(+ x 13)` em vez de `x + 13`.
# A mesma notação prefixa é usada para chamadas de funções—ex. `(gcd x 13)`—e
# instruções especiais—ex. `(define x 13)`, que corresponde à
# instrução de atribuição em Python: `x = 13`.
#
# A notação usada em Scheme e na maioria dos dialetos de Lisp (como Clojure) é chamada de _S-expression_ ou _expressão-S_.
#
# Eis um exemplo simples em Scheme, para calcular o máximo divisor comum:
#
#
# ```lisp
# (define (resto m n)
#     (- m (* n (quotient m n))))
#
# (define (mdc m n)
#     (if (= n 0)
#         m
#         (mdc n (resto m n))))
#
# (display (mdc 18 45))
# ```

# O mesmo algoritmo em Python:

# +
def resto(m, n):
    return m - (m // n * n)

def mdc(m, n):
    if n == 0:
        return m
    else:
        return mdc(n, resto(m, n))

print(mdc(18, 45))
# -

# > **DICA**: Clique na célua acima para selecioná-la, então tecle `【CTRL】【ENTER】` para executá-la.
# <br>O resultado aparecerá abaixo da célula.
#
# Scheme não tem estruturas de laço como `while` ou `for`.
# Iteração é feita através de recursão.
# Note como não há atribuições nos exemplos em Scheme ou Python acima.
# O uso extensivo de recursão e o uso reduzido de atribuição
# são duas características típicas de programação em um estilo funcional.
#
# Em Python idiomático eu usaria o operador `%` em vez de reinventar `resto`,
# e seria mais eficiente usar um laço `while` do que recursão.
# Mas eu queria mostrar duas definições de funções, e
# que os exemplos ficassem parecidos para ajudar você a ler o código em Scheme.
#
# Agora vamos estudar o código de uma versão de `lis.py` para Python 3.7.
# O código completo com testes para Python 3.10 você pode encontrar no diretório
# [18-with-match/lispy/py3.10/](https://github.com/fluentpython/example-code-2e/tree/master/18-with-match/lispy/py3.10/)
# do repositório [fluentpython/example-code-2e](https://github.com/fluentpython/example-code-2e).

# + [markdown] tags=[]
# ## Imports e tipos
#
# O código escrito pelo Norvig não usa anotações de tipo, adicionei as anotações e fiz mais algumas pequenas mudanças.
#
# Esse notebook usa Python 3.7 para rodar no [Binder](https://mybinder.org/), portanto precisamos importar alguns tipos de coleções do módulo `typing`.
#
# > **DICA**: Clique na célula abaixo para selecioná-la e então aperte `【SHIFT】【ENTER】` para executá-la e selecionar a próxima célula.<br>Use `【CTRL】【ENTER】` para executar a célula e manter ela selecionada.<br>Use esses comandos para executar as células conforme você segue.
# -

import sys
assert sys.version_info >= (3, 7), f'Esperado Python ≥ 3.7; instalado: {sys.version}'
sys.version

# +
################ lis.py: Scheme Interpreter in Python 3.10
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
## Type hints and minor additions by Luciano Ramalho

import math
import operator as op
from collections import ChainMap
from itertools import chain
from typing import Any, NoReturn
from typing import Union, List, MutableMapping, Optional, Iterator

Symbol = str
Atom = Union[float, int, Symbol]
Expression = Union[Atom, List]

Environment = MutableMapping[Symbol, object]


# -

# Os tipos são definidos são:
#
# `Symbol`: É apenas um apelido para o tipo `str`. Em _list.py_, `Symbol` é utilizado pelos identificadores,
# não há nenhum tipo que seja string com operaçoẽs como slicing, splitting etc.
#
# `Atom`: Um elemento simples de sintaxe como um número ou um `Symbol`, um `Atom` é o contrário de uma estrutura composta de diversas partes como uma lista.
#
# `Expression`: Programas escritos utilizando Scheme são compostos por expressões feitas com `Atoms` e listas, que provavelmente estarão aninhadas.
#
# > **NOTA**: O segundo interpretador escrito pelo Norvigs,
# [`lispy.py`](https://github.com/fluentpython/example-code-2e/blob/master/18-with-match/lispy/original/lispy.py),
# suporta string como um tipo, assim como também aceitar funcionalidades avançadas como macros de sintax,
# continuations e tail calls.
# No entanto, `lispy.py` é quase três vezes mais longo do que o `lis.py` e mais difícil de entender.

# ## The Parser
#
# Norvig's parser is 36 lines of code showcasing the power of Python applied to handling the
# simple recursive syntax of S-expression—without string data,
# comments, macros, and other features of standard Scheme that make parsing more complicated (these features are implemented in `lispy.py`).

# +
def parse(program: str) -> Expression:
    "Read a Scheme expression from a string."
    return read_from_tokens(tokenize(program))

def tokenize(s: str) -> List[str]:
    "Convert a string into a list of tokens."
    return s.replace('(', ' ( ').replace(')', ' ) ').split()

def read_from_tokens(tokens: List[str]) -> Expression:
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        exp = []
        while tokens[0] != ')':
            exp.append(read_from_tokens(tokens))
        tokens.pop(0)  # discard ')'
        return exp
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return parse_atom(token)

def parse_atom(token: str) -> Atom:
    "Numbers become numbers; every other token is a symbol."
    try:
        return int(token)
    except ValueError:
        try:
            return float(token)
        except ValueError:
            return Symbol(token)


# -

# The main function of that group is `parse` which takes an S-expressions as a `str`
# and returns an `Expression` object: an `Atom` or a `list` that may contain more atoms and nested lists.
#
# Norvig uses a smart trick in `tokenize`:
# he adds spaces before and after each parenthesis in the input and then splits it,
# resulting in a list of syntactic tokens with `'('` and `')'`
# as separate tokens.
# This shortcut works because there is no string type in the little Scheme of _lis.py_, so every `'('` or `')'` is an expression delimiter.
# The recursive parsing code is in `read_from_tokens`.
# I will not explain it now because I want to focus on the other parts of the interpreter.
#
# Below are some examples of the top-level `parse` function.
#
# > **TIP**:  To run the code in each cell and select the next, use `【SHIFT】【ENTER】`.<br>
# If you get `NameError: name 'parse' is not defined`, use the menu command ***Cell > Run All Above***.

parse('1.5')

parse('ni!')

parse('''
  (define double
    (lambda (n)
      (* n 2)))
''')

# The parsing rules for this subset of Scheme are simple:
#
# 1. A token that looks like a number is parsed as a `float` or `int`.
# 2. Anything else that is not `'('` or `')'` is parsed as a `Symbol`—a `str` to be used as an identifier. This includes source text like `+`, `set!`, and `make-counter` that are valid identifiers in Scheme but not in Python.
# 3. Expressions inside `'('` and `')'` are recursively parsed as lists containing atoms or nested lists that may contain atoms and more nested lists.
#
# Using terminology of the Python interpreter, the output of `parse` is an **AST** (Abstract Syntax Tree):
# a convenient representation of the Scheme program as nested lists forming a tree-like structure,
# where the outermost list is the trunk, inner lists are the branches, and atoms are the leaves.

# ### Exercise 0
#
# Replace the ellipis `...` with the AST for the given S-expressions, to make the comparison `True`.
# To run the code in the cell, hit `【CTRL】【ENTER】`. 

parse('9') == ...

parse('x/y') == ...

parse('(+ 3 7)') == ...

parse('(* c (/ 9 5))') == ...

parse('(+ 32 (* (/ 9 5) c ))') == ...


# ## Built-in Environment for a Calculator
#
#
# The `standard_env()` function builds and returns an `Environment` loaded
# with predefined functions, similar to Python's `__builtins__` module that is always available.

def standard_env() -> Environment:
    "An environment for an s-expression calculator."
    env: Environment = {}
    env.update(vars(math))   # sin, cos, sqrt, pi, ...
    env.update(
        {
            '+': op.add,
            '-': op.sub,
            '*': op.mul,
            '/': op.truediv,
            'quotient': op.floordiv,
            '>': op.gt,
            '<': op.lt,
            '>=': op.ge,
            '<=': op.le,
            '=': op.eq,
            'abs': abs,
            'begin': lambda *x: x[-1],
            'eq?': op.is_,
            'equal?': op.eq,
            'max': max,
            'min': min,
            'not': op.not_,
            'number?': lambda x: isinstance(x, (int, float)),
            'procedure?': callable,
            'round': round,
            'symbol?': lambda x: isinstance(x, Symbol),
        }
    )
    return env


# The `env` mapping is loaded with:
#
# * all functions from Python's `math` module;
# * selected operators from Python's `op` module;
# * simple but powerful functions built with Python's `lambda`;
# * Python built-ins renamed like `callable` as `procedure?` or directly mapped like `round`.

# ## A Calculator
#
# This first version of `evaluate` handles expressions using built-in functions and user-defined variables.
#
# > **NOTE**: Norvig's parser is simple and solid, but his evaluator is simple and fragile. He ommited error checking to keep the logic easy to follow. In his words: "Lispy does not attempt to detect, reasonably report, or recover from errors. Lispy expects the programmer to be perfect." ([source](https://norvig.com/lispy.html)).
#

def evaluate(x: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    if isinstance(x, Symbol):                    # variable reference
        return env[x]
    elif not isinstance(x, list):                # constant literal
        return x
    elif x[0] == 'define':                       # (define var exp)
        _, var, exp = x
        env[var] = evaluate(exp, env)
    else:                                        # (proc arg...)
        proc_exp, *args = x
        proc = evaluate(proc_exp, env)
        arg_values = [evaluate(exp, env) for exp in args]
        return proc(*arg_values)


# Run these examples to see `evaluate` in action.
#
# A curious square:

evaluate(parse('(* 11111 11111)'), standard_env())

# If there are 876 candidates, and 123 were approved, what percentage was approved?

evaluate(parse('(* (/ 123 876) 100)'), standard_env())

# Now let's study each part of the `if/elif/…` in `evaluate`.

# ### Evaluate symbol
#
# ```python
#     if isinstance(x, Symbol):
#         return env[x]
# ```
#
# If the expression is a `Symbol`, then look it up in the environment.
#
#

evaluate('pi', standard_env())

evaluate('+', standard_env())

# ### Evaluate other atoms
#
# ```python
#     elif not isinstance(x, list):
#         return x
# ```
#
# If the expression is not a `list` and not a `Symbol` (because of the preceding check), then assume it is a constant literal and return it as is.

evaluate(1.5, standard_env())

# ### Evaluate `(define var exp)`
#
# ```python
#     elif x[0] == 'define':
#         _, var, exp = x
#         env[var] = evaluate(exp, env)
# ```
#
# If the expression is a `list` starting with the keyword `define`, then it should be followed by a `Symbol` and an `Expression`. Recursively evaluate the expression in environment, and store it in `env` using the `Symbol` as key.

env = standard_env()
evaluate(parse('(define answer (* 7 6))'), env)
env['answer']

# ### Evaluate function call `(proc arg…)`
#
# ```python
#     else:
#         proc_exp, *args = x
#         proc = evaluate(proc_exp, env)
#         arg_values = [evaluate(exp, env) for exp in args]
#         return proc(*arg_values)
# ```
#
# If the expression is a `list` that does not start with a keyword, then:
#
# 1. Evaluate the first expression—it should return a procedure (a.k.a. function).
# 2. Evaluate the remaining expressions (the argument values)
# 3. Call the procedure with the argument values.

evaluate(['quotient', 8, 3], standard_env())

evaluate(['*', ['/', 123, 876], 100], standard_env())

# `evaluate` can process deeply nested expressions, but only one expression at the top level. To bundle several expressions into one, use the `(begin ...)` function. All the arguments after `begin` are evaluated before `begin` is called, and `begin` returns the value of the last argument. For example:

env = standard_env()
percent = """
(begin
  (define a 126)
  (define b (* 6 50))
  (* (/ a b) 100)
)
"""
evaluate(parse(percent), env)

# After the previous code, `env` now holds two variables: `a` and `b`.

env['a'], env['b']


# The `run()` function evaluates a string of one or more S-expressions as a program.

# ## Execução não-interativa com `run()`
#
# As funções a seguir pegam o código fonte do Scheme como string e executam.

# +
def run_lines(source: str, env: Optional[Environment] = None) -> Iterator[Any]:
    global_env: Environment = ChainMap({}, standard_env())
    if env is not None:
        global_env.update(env)
    tokens = tokenize(source)
    while tokens:
        exp = read_from_tokens(tokens)
        yield evaluate(exp, global_env)


def run(source: str, env: Optional[Environment] = None) -> Any:
    for result in run_lines(source, env):
        pass
    return result


# -

# Com `run()` nós não precisamos do `(begin …)` para avaliar diversas expressões - mas o `begin` ainda é útil em outras situações.

percent = """
(define a 126)
(define b (* 6 50))
(* (/ a b) 100)
"""
run(percent)

# ### Exercício 1
#
# Essa é a fórmula para converter temperaturas de Celsius para Fahrenheit:
#
# `f = (9 / 5) * c + 32`
#
# Aqui está o código para converter 20°C para °F:
#

c_to_f = """
(define c 20)
(+ 32 (* c (/ 9 5)))
"""
run(c_to_f)

# A função da conversão inversa é:
#
# `c = (f − 32) * 5 / 9`
#
# No código abaixo, substitua `(+ 1 2)` com a expressão para converter 68°F para °C.

f_to_c = """
(define f 68)
(+ 1 2)
"""
run(f_to_c)

# ### Exercicio 2
#
# O módulo `math` do Python inclui uma função `factorial`, que é parte do ambiente retornardo pelo `standard_env`:

run('(factorial 10)')

# O Scheme aceita `!` como um identificador. Sua tarefa é fazer o `factorial` do Python ficar disponível por meio do símbolo `!` no Scheme. 
#
# **Passo 2.1** Descomente a expressão abaixo e execute para ver um erro. Olhe para a última linha da saída do erro. Qual é o erro? Você entende porque esse é o erro?

# +
# run('(! 10)') == 3628800
# -

# **Passo 2.2.** Edite a função `standard_env` acima para adicionar uma entrada para `!`,
# mapeando para a função `math.factorial` do Python. 
#
# **Passo 2.3.** Execute a exressão acima para confirmar que o resultado é `True`. 

# ### Exercício 3
#
# Em um Scheme padrão, o operador `+` é variável, ou seja, aceita qualquer número de argumentos. 
# Com 0 argumentos, `(+)` retorna `0`; caso sontrário retorna a soma de todos os argumentos. 
#
# **Step 3.1.** Descomente a expressão abaixo e execute para ver um erro. O erro faz sentido para você?

# +
# run('(= 10 (+ 1 2 3 4))')
# -

# **Passo 3.2.** Edite a função `standard_env` acima para reimplementar o `+` para fazer a expressão acima retornar `True`.
#
# > **DICA 3.2.1**: escondida abaixo está uma versão variável do `sum` do Python. Considere resolver o exercício sem revelar a dica. Para revelar o código, descomente a linha do `print()` e execute a célula.
#

from base64 import b64decode
blob = (b'ZGVmIHZhcmlhZGljX3N1bSgqYXJncyk6CiAgICByZXR1cm4g'
        b'c3VtKGFyZ3MpCgp2YXJpYWRpY19zdW0oMSwgMiwgMywgNCk=')
# print(b64decode(blob).decode('utf8'))

# > **DICA 3.2.2**: a mesma função em uma única linha do Python. Tente resolver o exercício sem revelar a dica. Para revelar, descomente a linha do `print()` e execute a célula.

blob = b'ZiA9IGxhbWJkYSAqYXJnczogc3VtKGFyZ3MpCmYoMSwgMiwgMywgNCk='
# print(b64decode(blob).decode('utf8'))

# ## A more complete environment

# This new version of `standard_env()` defines functions that handle lists.

def standard_env() -> Environment:
    "An environment with some Scheme standard procedures."
    env: Environment = {}
    env.update(vars(math))   # sin, cos, sqrt, pi, ...
    env.update(
        {
            '+': op.add,
            '-': op.sub,
            '*': op.mul,
            '/': op.truediv,
            'quotient': op.floordiv,
            '>': op.gt,
            '<': op.lt,
            '>=': op.ge,
            '<=': op.le,
            '=': op.eq,
            'abs': abs,
            'append': lambda *args: list(chain(*args)),          
            'apply': lambda proc, args: proc(*args),
            'begin': lambda *x: x[-1],
            'car': lambda x: x[0],
            'cdr': lambda x: x[1:],
            'cons': lambda x, y: [x] + y,
            'eq?': op.is_,
            'equal?': op.eq,
            'filter': lambda *args: list(filter(*args)),
            'length': len,
            'list': lambda *x: list(x),
            'list?': lambda x: isinstance(x, list),
            'map': lambda *args: list(map(*args)),
            'max': max,
            'min': min,
            'not': op.not_,
            'null?': lambda x: x == [],
            'number?': lambda x: isinstance(x, (int, float)),
            'procedure?': callable,
            'round': round,
            'symbol?': lambda x: isinstance(x, Symbol),
        }
    )
    return env


# ## `Procedure`: a class to represent a closure
#
# The next improvement to `evaluate()` will include the `(lambda …)` form to allow user-defined functions (the authors of Scheme prefer the term *procedure*). To support `lambda`, we need a class to represent a procedure:

class Procedure:
    "A user-defined Scheme procedure."

    def __init__(self, parms: List[Symbol], body: Expression, env: Environment):
        self.parms = parms
        self.body = body
        self.env = env

    def __call__(self, *args: Expression) -> Any:
        local_env = dict(zip(self.parms, args))
        env: Environment = ChainMap(local_env, self.env)
        return evaluate(self.body, env)


# The `Procedure` class could very well be named `Closure`,
# because that's what it represents:
# a function definition together with an environment captured when the function is defined.
# The required parameters to create a `Procedure` are:
#
# `parms`: the function parameter names as a list of symbols. This list may be empty.
#
# `body`: the body of the function as an expression to be evaluated when the function is invoked.
#
# `env`: the environment where the function is created. This is what makes it a [*closure*](https://en.wikipedia.org/wiki/Closure_(computer_programming)).
#
# The `__init__` method simply stores the arguments passed. None of them is evaluated when the function is defined.
#
# The environment is used when the function is called to provide the values of
# the [*non-local variables*](https://en.wikipedia.org/wiki/Non-local_variable):
# variables that appear in the body of the function but that
# are not parameters or local variables.
#
# Let's create a `Procedure` "by-hand" to see how it works:

double = Procedure(['n'], ['*', 'n', 2], standard_env())
double(4)


# ## Evaluate with `lambda`, `if`, and `quote`
#
# To transform the calculator into a worthy subset of Scheme, we need to support user defined functions, conditionals and the `(quote …)` form to handle S-expressions as data—instead of evaluating them.
#

def evaluate(x: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    if isinstance(x, str):                       # variable reference
        return env[x]
    elif not isinstance(x, list):                # constant literal
        return x
    elif x[0] == 'define':                       # (define var exp)
        _, var, exp = x
        env[var] = evaluate(exp, env)
    elif x[0] == 'lambda':                       # (lambda (var...) body)
        _, parms, body = x
        return Procedure(parms, body, env)
    elif x[0] == 'quote':                        # (quote exp)
        _, exp = x
        return exp
    elif x[0] == 'if':                           # (if test consequence alternative)
        _, test, consequence, alternative = x
        if evaluate(test, env):
            return evaluate(consequence, env)
        else:
            return evaluate(alternative, env)
    else:                                        # (proc arg...)
        proc_exp, *args = x
        proc = evaluate(proc_exp, env)
        arg_values = [evaluate(exp, env) for exp in args]
        return proc(*arg_values)


# ### Evaluate `(lambda (var…) body)`
#
# ```python
#     elif x[0] == 'lambda':                       
#         _, parms, body = x
#         return Procedure(parms, body, env)
# ```
#
# If the expression is a `list` starting with the keyword `lambda`, followed by a list of 0 or more symbols, and a single `body` expression, then build and return a `Procedure`.
#
# Example:

percent = run('(lambda (a b) (* (/ a b) 100))')
percent(15, 20)

# The result of `(lambda …)` is an anonymous function, so it is not stored in the environment. To create a named function, use `lambda` with `define`.
#
# > **NOTE**: This version of `lis.py` only accepts a single expression as the `body` of the function. Use `(begin …)` to wrap multiple expressions. The result of the function will be the value of the last expression.

# ### Evaluate `(quote exp)`
#
# ```python
#     elif x[0] == 'quote':
#         _, exp = x
#         return exp
# ```
#
# If the expression is a `list` starting with the keyword `quote` followed by a single expression, return the expression without evaluating it.
#
# Examples:

run('(quote no-such-name)')  # undefined symbol, would raise error if evaluated

run('(quote (99 bottles of beer))')  # 99 is not the name of a function or reserved word

run('(quote (/ 10 0))')  # this would raise division by zero if evaluated

# ### Evaluate `(if test consequence alternative)`
#
# ```python
#     elif x[0] == 'if':
#         _, test, consequence, alternative = x
#         if evaluate(test, env):
#             return evaluate(consequence, env)
#         else:
#             return evaluate(alternative, env)
# ```
#
# If the expression is a `list` starting with the keyword `if`, followed by exactly three expressions, evaluate `test`. If true, evaluate `consequence`; otherwise evaluate `alternative`.
#
# Example:

run('(if (= 3 3) 1 0)')

run('(if (= 3 30) 1 0)')

source = '''
(define pass-fail
    (lambda (grade)
        (if (>= grade 5)
            (quote PASS)
            (quote FAIL))))
(pass-fail 7)
'''
run(source)


# ## The REPL
#
# Norvig's REPL (Read-Eval-Print-Loop) is easy to undersand but not user-friendly.
# If no command-line arguments are given to _lis.py_,
# the `repl()` function is invoked by `main()`—defined at the end of the module.
# At the `lis.py>` prompt we must enter correct and complete expressions—if
# we forget to close one parenthesis, _lis.py_ crashes.
#
# > **NOTE**: As I studied Norvig's _lis.py_ and _lispy.py_, I started a fork named
#   [`mylis`](https://github.com/fluentpython/lispy/blob/main/mylis)
#   which adds some features, including a REPL that accepts partial S-expressions
#   and prompts for the continuation, similar to how Python's REPL
#   knows we are not finished and presents the secondary prompt `...` until
#   we enter a complete expression or statement that can be evaluated.
#   `mylis` also handles a few errors gracefully, but it's still easy to crash.
#

# +
def repl(prompt: str = 'lis.py> ') -> NoReturn:
    "A prompt-read-evaluate-print loop."
    global_env: Environment = standard_env()
    while True:
        val = evaluate(parse(input(prompt)), global_env)
        if val is not None:
            print(lispstr(val))


def lispstr(exp: object) -> str:
    "Convert a Python object back into a Lisp-readable string."
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')'
    else:
        return str(exp)


# -

# Function `repl` calls `standard_env()` to provide built-in functions for the global environment,
# then enters an infinite loop reading and parsing each input line,
# evaluating it in the global environment and displaying the result—unless it's `None`.
# The `global_env` may be modified by `evaluate`.
#
# `lispstr` is the inverse function of `parse`:
# given a Python object representing an expression,
# `parse` returns the Scheme source code for it.
# For example:

lispstr(['+', 32, ['*', ['/', 9, 5], 'c']])

# ## Examples
#
# ### Simple recursive factorial

fact_src = '''
(define ! (lambda (n)
    (if (< n 2) 
        1 
        (* n (! (- n 1)))
)))

(! 5)
'''
run(fact_src)

# ### Quicksort
#
# Tony Hoare's elegant [recursive sorting algorithm](https://en.wikipedia.org/wiki/Quicksort).
#
# Note the use of `quote` to create a list of numbers, and the use of Scheme list handling functions: `null?`, `car`, `cdr`, `append`, `list`, and `filter`.

quicksort_src = """
(define quicksort (lambda (lst)
    (if (null? lst)
        lst
        (begin
            (define pivot (car lst))
            (define rest (cdr lst))
            (append
                (quicksort
                    (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quicksort
                    (filter (lambda (x) (>= x pivot)) rest)))
))))

(quicksort (quote (2 1 6 3 4 0 8 9 7 5)))
"""
run(quicksort_src)

# ### Square root algorithm
#
# This is known as the [Babylonian method](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method),
# an ancient special case of [Newton's method](https://en.wikipedia.org/wiki/Newton%27s_method).
# This code is adapted from an [example](https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node12.html) in the book *Structure and Interpretation of Computer Programs*.

sqrt_src = """
(define sqrt (lambda (x)
    (sqrt-iter 1.0 x)))
    
(define sqrt-iter (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))
        
(define good-enough? (lambda (guess x)
    (< (abs (- (* guess guess) x)) 0.001)))
    
(define improve (lambda (guess x)
    (average guess (/ x guess))))
    
(define average (lambda (x y)
    (/ (+ x y) 2)))
    
(sqrt 123454321)
"""
run(sqrt_src)

# ### Tail-recursive factorial
#
# The `factorial-iter` function is tail-recursive: the recursive call is returned as the result. That's a
# [*tail call*](https://en.wikipedia.org/wiki/Tail_call).
#
# In contrast, the [Simple recursive factorial](#Simple-recursive-factorial) is not tail recursive: the result of the recursive call is multiplied by `n` before it is returned.

fact_src = '''
(define ! (lambda (n)
    (factorial-iter n 1)))

(define factorial-iter
    (lambda (n product)
        (if (= n 1)
            product
            (factorial-iter (- n 1) (* n product))
        )
    )
)
      
(! 5)
'''
run(fact_src)

# > **NOTE**: `lis.py` does not implement proper tail calls (PTC)—a.k.a. tail call optimization (TCO).
# Therefore, there is no advantage in writing tail recursive functions. But
# [`lispy.py`](https://github.com/norvig/pytudes/blob/main/py/lispy.py) and
# [`mylis_2`](https://github.com/fluentpython/lispy/blob/main/mylis/mylis_2/lis.py) implement PTC, so tail-recursion does not grow the stack, and tail-recursive code is more efficient.

# ### Greatest common divisor
#
# The [Euclidean algorihm](https://en.wikipedia.org/wiki/Euclidean_algorithm).
#
# > **NOTE**: This is example uses `lambda` inside `define` instead of the shortcut `define` form listed in 
# [Scheme Syntax](#Scheme-Syntax), which creates named procedures directly. See discussion below.

gcd_src = '''
(define mod (lambda (m n)
    (- m (* n (quotient m n)))))

(define gcd (lambda (m n)
    (if (= n 0)
        m
        (gcd n (mod m n)))))

(gcd 18 45)
'''
run(gcd_src)

# ## Syntactic sugar
#
# Standard Scheme provides an alternative syntax for `define` that allows defining named functions without `lambda`.
#
# The syntax is: `(define (name parms…) body…)`, where:
#
# `name`: the name of the function to be defined (a `Symbol`);
#
# `parms…`: 0 or more symbols declaring the parameter names;
#
# `body…`: 1 or more expressions to be used as the body of the function.
#
# That's is an example of _syntactic sugar_: new syntax that does not add any functionality to the language, but makes it more convenient to use.
#
# The version of `gcd` shown in [Scheme Syntax](#Scheme-Syntax) uses that shortcut syntax. Here it is again:
#
# ```lisp
# (define (mod m n)
#     (- m (* n (quotient m n))))
#
# (define (gcd m n)
#     (if (= n 0)
#         m
#         (gcd n (mod m n))))
#
# (gcd 18 45)
# ```
#
# ### Take-home exercise
#
# Check your understanding of `lis.py` by implementing the shortcut syntax of `define` in the `evaluate()` function.
# Test your work by running the example below. The result should be 9.
#

gcd2_src = '''
(define (mod m n)
    (- m (* n (quotient m n))))

(define (gcd m n)
    (if (= n 0)
        m
        (gcd n (mod m n))))

(gcd 18 45)
'''
# run(gcd2_src)


