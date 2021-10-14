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
# * [Imports e tipos](#Imports-e-tipos)
# * [O parser](#O-parser)
#   * [Exercício 0](#Exercício-0)
# * [Ambiente básico para aritmética](#Ambiente-básico-para-aritmética)
# * [A Calculator](#A-Calculator)
# * [Execução não interativa](#Execução-não-interativa)
#   * [Exercício 1](#Exercício-1)
#   * [Exercício 2](#Exercício-2)
#   * [Exercício 3](#Exercício-3)
# * [User defined procedures](#User-defined-procedures)
# * [Um ambiente mais completo](#Um-ambiente-mais-completo)
# * [`Procedure`: a class to represent a closure](#Procedure:-a-class-to-represent-a-closure)
# * [Evaluate with `lambda`, `if`, and `quote`](#Evaluate-with-lambda,-if,-and-quote)
# * [The REPL](#The-REPL)
# * [Exemplos](#Exemplos)
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

# > **DICA**: Clique na célula acima para selecioná-la, então tecle `【CTRL】【ENTER】` para executá-la.
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
# `Symbol`: Apenas um apelido para o tipo `str`.
# Em _list.py_, instâncias de `Symbol` são usadas como identificadores;
# não há um tipo string com operaçoẽs como fatiamento, particionamento com `split` etc.
#
# `Atom`: Elemento sintático simples: um número ou um `Symbol`.
# Um átomo é o contrário de uma estrutura composta de diversas partes como uma lista.
#
# `Expression`: Programas em Scheme são formados por expressões feitas com átomos e listas, possivelmente aninhadas.
#
# > **NOTA**: O segundo interpretador escrito por Norvig,
# [`lispy.py`](https://github.com/fluentpython/example-code-2e/blob/master/18-with-match/lispy/original/lispy.py),
# suporta string como um tipo de dado, assim como também aceita funcionalidades avançadas como macros de sintaxe,
# chamadas de cauda eficientes e _continuations_.
# No entanto, `lispy.py` é quase três vezes mais longo do que `lis.py`, e mais difícil de entender.

# ## O parser
#
# O parser de Norvig são 36 linhas de código que demonstram o poder de Python aplicado ao manuseio de
# sintaxes recursivas simples de S-expression—sem strings como dados,
# comentários, macros, e outros recursos de Scheme padrão que complicam a análise sintática (esses recursos são implementadas em `lispy.py`).

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

# A função principal desse grupo é `parse`, que toma uma S-expression como uma `str`
# e devolve um objeto `Expression`: um `Atom` ou `list` que pode conter mais átomos e listas aninhadas.
#
# Norvig usa um truque inteligente em `tokenize`:
# ele adiciona espaços antes e depois de cada parênteses no input e depois divide,
# resultando em uma lista de tokens sintáticos com `(` e `)`
# como tokens distintos.
# Esse atalho funciona porque não existe um tipo de string no pequeno Scheme do _lis.py_, então todo `(` ou `)` é um delimitador de expressão.
# O código parsing recursivo está em `read_from_tokens`.
# Eu não vou explicar isso agora porque quero focar nas outras parter do interpretador.
#
# Abaixo, estão alguns exemplos do nível mais alto da função `parse`.
#
# > **DICA**: Para executar o código em cada uma das células e selecionar a próxima, use `【SHIFT】【ENTER】`.<br>
# Se acontecer `NameError: name 'parse' is not defined`, use o comando ***Cell > Run All Above*** do menu para executar as células acima, incluindo aquela onde está a definição da função `parse`.

parse('1.5')

parse('ni!')

parse('''
  (define double
    (lambda (n)
      (* n 2)))
''')

# As regras do parsing para esse subconjunto do Scheme são simples:
#
# 1. Um token que se parece com um número é parseado como um `float` ou `int`.
# 2. Qualquer outra coisa que não for `(` ou `)` é parseado como um `Symbol` - uma `str` a ser usada como identificador. Isso inclui texto fonte como `+`. `set` e `make-counter` que são identificadores válidos em Scheme mas não em Python.
# 3. Expressões dentro de `(` e `)` são recursivamente parseadas como listas contendo atoms ou listas aninhadas que podem conter atoms e mais listas aninhadas.
#
# Usando a terminologia do interpretador Python, a saída de `parse` é uma **AST** (*Abstract Syntax Tree* ou *Árvore Sintática Abstrata*):
# uma representação conveniente do programa Scheme como listas aninhadas formando uma estrutura em forma de árvore,
# onde a lista mais externa é o tronco, as listas internas são ramos e os átomos são folhas.
#
# Veja a AST do exemplo `(define double (lambda (n) (* n 2)))` como um diagrama em árvore:
#
# ```
#                               '*'  'n'   2
#                         'n'    └────┼────┘
#                          │          │
#            'lambda'     [ ]        [ ]
#                └─────────┼──────────┘
#                          │
# 'define'   'double'     [ ]
#     └─────────┼──────────┘
#               │
#              [ ]
# ```
#
# ### Exercício 0
#
# Substitua as reticências `...` com a AST correspondente à cada S-expression, para obter o resultado `True`.
# Para rodar o código na célula, tecle `【CTRL】【ENTER】`.

parse('9') == ...

parse('x/y') == ...

parse('(+ 3 7)') == ...

parse('(* c (/ 9 5))') == ...

parse('(+ 32 (* (/ 9 5) c ))') == ...


# ## Ambiente básico para aritmética
#
# A função `standard_env()` constrói e devolve um `Environment` carregado
# com funções pré definidas, similar ao módulo `__builtins__` do Python que está sempre disponível.

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


# O mapeamento `env` é carregado com:
#
# * todas as funções do módulo `math` do Python;
# * operadores selecionados do módulo `op` do Python;
# * simples mas poderosas funções construídas com `lambda` do Python;
# * built-ins do Python renomeados, por exemplo `callable` como `procedure?` ou diretamente mapeados como `round`.

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

# ## Execução não-interativa
#
# As funções a seguir aceitam o código fonte de um programa em Scheme como uma string e o executam.

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

# Com `run()` não precisamos de `(begin …)` para avaliar múltiplas expressões de uma vez só—mas
# `begin` ainda é útil em outras situações.

porcento = """
(define a 126)
(define b (* 6 50))
(* (/ a b) 100)
"""
run(porcento)

# ### Exercício 1
#
# Essa é a fórmula para converter temperaturas de Celsius para Fahrenheit:
#
# `f = (9 / 5) * c + 32`
#
# Eis o código para converter 20°C para °F:

c_to_f = """
(define c 20)
(+ 32 (* c (/ 9 5)))
"""
run(c_to_f)

# A função inversa é:
#
# `c = (f − 32) * 5 / 9`
#
# No código abaixo, substitua `(+ 1 2)` pela expressão correta para converter 68°F para °C.

f_to_c = """
(define f 68)
(+ 1 2)
"""
run(f_to_c)

# ### Exercicio 2
#
# O módulo `math` de Python inclui uma função `factorial`, que é parte do ambiente criado por `standard_env()`:

run('(factorial 10)')

# A linguagem Scheme aceita `!` como um identificador. Sua tarefa é fazer `factorial()` de Python ficar disponível através do símbolo `!` em Scheme.
#
# **Passo 2.1** Descomente a expressão abaixo e execute para ver o erro. Veja para a última linha da saída do erro. Qual é o erro? Você entende porque esse é o erro?

# +
# run('(! 10)') == 3628800
# -

# **Passo 2.2.** Edite a função `standard_env` acima para adicionar uma entrada para `'!'`,
# mapeando para a função `math.factorial` de Python.
#
# **Passo 2.3.** Execute a expressão acima para confirmar que o resultado é `True`.

# ### Exercício 3
#
# Em um Scheme padrão, o operador `+` é variádico, ou seja, aceita qualquer quantidade de argumentos.
# Com 0 argumentos, `(+)` retorna `0`; caso contrário retorna a soma de todos os argumentos.
#
# **Step 3.1.** Descomente a expressão abaixo e execute para ver o erro. O erro faz sentido para você?

# +
# run('(= 10 (+ 1 2 3 4))')
# -

# **Passo 3.2.** Edite a função `standard_env` acima para reimplementar o `+`, fazendo a expressão acima devolver `True`.
#
# > **DICA 3.2.1**: Escondida abaixo está uma versão variável do `sum` do Python. Considere resolver o exercício sem revelar a dica. Para revelar o código, descomente a linha do `print()` e execute a célula.
#

from base64 import b64decode
blob = (b'ZGVmIHZhcmlhZGljX3N1bSgqYXJncyk6CiAgICByZXR1cm4g'
        b'c3VtKGFyZ3MpCgp2YXJpYWRpY19zdW0oMSwgMiwgMywgNCk=')
# print(b64decode(blob).decode('utf8'))

# > **DICA 3.2.2**: A seguir, a mesma função em uma única linha do Python. Tente resolver o exercício sem revelar a dica. Para revelar, descomente a linha do `print()` e execute a célula.

blob = b'ZiA9IGxhbWJkYSAqYXJnczogc3VtKGFyZ3MpCmYoMSwgMiwgMywgNCk='
# print(b64decode(blob).decode('utf8'))

# ## Um ambiente mais completo

# Esta nova versão de `standard_env()` define funções para trabalhar com listas.

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

# ## Exemplos
#
# ### Fatorial recursivo simples

fatorial_scm = '''
(define ! (lambda (n)
    (if (< n 2)
        1
        (* n (! (- n 1)))
)))

(! 5)
'''
run(fatorial_scm)

# ### Fatorial com recursão de cauda

fatorial_scm = '''
(define ! (lambda (n)
    (fatorial-iter n 1)))

(define fatorial-iter
    (lambda (n produto)
        (if (= n 1)
            produto
            (fatorial-iter (- n 1) (* n produto))
        )
    )
)

(! 5)
'''
run(fatorial_scm)

# A função `fatorial-iter` faz recursão de cauda:
# a chamada recursiva é devolvida como resultado diretamente.
# Isso é denominado uma chamada de cauda ou [*tail call*](https://en.wikipedia.org/wiki/Tail_call).
#
# Em contraste, o [Fatorial recursivo simples](#Fatorial-recursivo-simples)
# não é um exemplo de recursão de cauda:
# o resultado da chamada recursiva é multiplicado por `n` antes de ser devolvido.
#
# O sufixo `-iter` é comumente usado em Scheme para funções que fazem iteração por recursão de cauda. É comum que tais funções utilizem um parâmetro acumulador, que vai gradualmente acumulando resultados parciais. Em `fatorial-iter`, o parâmetro `produto` é o acumulador.
#
# > **NOTA**: `lis.py` não implementa chamadas de cauda eficientes, um recurso conhecido em inglês como _proper tail call_ (PTC) ou _tail call optimization_ (TCO), conforme o autor.
# Portanto, não há vantagem em fazer recursão de cauda. Porém
# [`lispy.py`](https://github.com/norvig/pytudes/blob/main/py/lispy.py) e
# [`mylis_2`](https://github.com/fluentpython/lispy/blob/main/mylis/mylis_2/lis.py)
# implementam PTC, o que significa que nesses interpretadores uma recursão de cauda não faz a pilha crescer a cada iteração.

# ### Quicksort
#
# O [algoritmo recursivo de ordenação](https://pt.wikipedia.org/wiki/Quicksort) eficiente e elegante inventado por Tony Hoare.
#
# Observe na última linha o uso de `quote` para criar uma lista de números, e na função `quicksort` o uso de funções de tratamento de listas: `null?`, `car`, `cdr`, `append`, `list`, e `filter`.

quicksort_scm = """
(define quicksort (lambda (lst)
    (if (null? lst)
        lst
        (begin
            (define pivô (car lst))
            (define resto (cdr lst))
            (append
                (quicksort
                    (filter (lambda (x) (< x pivô)) resto))
                (list pivô)
                (quicksort
                    (filter (lambda (x) (>= x pivô)) resto)))
))))

(quicksort (quote (2 1 6 3 4 0 8 9 7 5)))
"""
run(quicksort_scm)

# > **NOTA**: o código acima funciona em `lis.py`, mas não em Scheme padrão. A forma `define` não pode ser usada naquele contexto em Scheme padrão; em vez dela, usaríamos a forma `let`—que não existe em `lis.py`.

# ### Raiz quadrada por aproximação
#
# O [método bibilônio](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method),
# um algoritmo para calcular raiz quadrada usado desde a antiguidade. É um caso especial do [Método de Newton–Raphson](https://pt.wikipedia.org/wiki/M%C3%A9todo_de_Newton%E2%80%93Raphson) para cálculo de raízes de funções.
# Este código foi adaptado de um [exemplo](https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node12.html) do livro *Structure and Interpretation of Computer Programs*.

raiz2_scm = """
(define raiz2 (lambda (x)
    (raiz2-iter 1.0 x)))

(define raiz2-iter (lambda (chute x)
    (if (próximo? chute x)
        chute
        (raiz2-iter (melhorar chute x) x))))

(define próximo? (lambda (chute x)
    (< (abs (- (* chute chute) x)) 0.001)))

(define melhorar (lambda (chute x)
    (média chute (/ x chute))))

(define média (lambda (x y)
    (/ (+ x y) 2)))

(raiz2 12345654321)
"""
run(raiz2_scm)

# ### Máximo divisor comum
#
# O [algoritmo de Euclides](https://pt.wikipedia.org/wiki/Algoritmo_de_Euclides).

mdc_scm = '''
(define resto (lambda (m n)
    (- m (* n (quotient m n)))))

(define mdc (lambda (m n)
    (if (= n 0)
        m
        (mdc n (resto m n)))))

(mdc 18 45)
'''
run(mdc_scm)

# > **NOTA**: Este exemplo usa `lambda` dentro de `define` em vez da sintaxe abreviada com `define`
# ilustrada na função `mdc` em [Sintaxe de Scheme](#Sintaxe-de-Scheme). Leia mais a seguir.

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


