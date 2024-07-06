# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.16.2
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %% [markdown]
# # O *lis.py* de Norvig
#
#
# ![Norvig's lispy](lispy.png)

# %% [markdown]
# Contents:
#
# * [Introdução](#Introdução)
# * [Sintaxe de Scheme](#Sintaxe-de-Scheme)
# * [Imports e tipos](#Imports-e-tipos)
# * [O parser](#O-parser)
#   * [Exercício 0](#Exercício-0)
# * [Ambiente básico para aritmética](#Ambiente-básico-para-aritmética)
# * [Uma calculadora](#Uma-calculadora)
# * [Execução não-interativa](#Execução-não-interativa)
#   * [Exercício 1](#Exercício-1)
#   * [Exercício 2](#Exercício-2)
#   * [Exercício 3](#Exercício-3)
# * [User defined procedures](#User-defined-procedures)
# * [Um ambiente mais completo](#Um-ambiente-mais-completo)
# * [`Procedure`: uma clase que representa uma _closure_](#Procedure:-uma-classe-que-representa-uma-closure)
# * [Avaliador com `lambda`, `if` e `quote`](#Avaliador-com-lambda,-if-e-quote)
# * [O REPL](#O-REPL)
# * [Exemplos](#Exemplos)
# * [Açúcar Sintático](#Açúcar-Sintático)
#   * [Exercício para casa](#Exercício-para-casa)
#
# > **LICENÇAS**:<br>
#   Código © 2010-2018 Peter Norvig, [MIT License](https://github.com/fluentpython/lispy/blob/main/LICENSE)<br>
#   Texto © 2022 Luciano Ramalho, [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/)
#

# %% [markdown]
# ## Introdução
#
# [Peter Norvig](https://norvig.com/) da universidade Stanford criou
# [*lis.py*](https://github.com/norvig/pytudes/blob/main/py/lis.py):
# um interpretador em 132 linhas de código Python legível,
# para parte da linguagem Scheme—um dialeto de Lisp.
#
# Porque você deveria estudar *lis.py*?
# Para mim esses foram alguns motivos:
#
# * Depois de estudar como funciona um interpretador,
# entendi mais precisamente como funciona Python e linguagens em geral—interpretadas ou compiladas.
#
# * A simplicidade de Scheme é uma aula magna de design de linguagens.
#
# * *lis.py* é um ótimo exemplo de código Python idiomático.
#
# Norvig descreve *lis.py* em um texto intitulado
# [(How to Write a (Lisp) Interpreter (in Python))](https://norvig.com/lispy.html).
# Leitura recomendada!
#
# Antes de examinar o código do interpretador em Python,
# vamos ver um pouco de Scheme—caso você nunca tenha visto essa linguagem ou Lisp antes.
#

# %% [markdown]
# ## Mágica para executar lis.py em células
#
# O comando "mágico" `%%lispy` executa o conteúdo da célula com o interpretador `lis.py` cujo fonte está no mesmo diretório deste notebook.
# Antes de usar o comando, é necessário importá-lo.
#
# Veremos o código por trás desse comando mágico mais tarde.
#
# > **DICA:** Clique na célula abaixo para selecioná-la e então tecle
#  `【SHIFT】【ENTER】` para executá-la e selecionar a próxima célula.<br>

# %%
from lis import lispy

# %%
# %%lispy

(* 6 7)

# %% [markdown]
# ## Sintaxe de Scheme
#
# Todo código Scheme é formado por expressões.
# Não existem operadores infixos:
# todas as expressões usam notação prefixa como
# `(+ x 13)` em vez de `x + 13`.
#
# A mesma notação prefixa é usada para chamadas de funções—ex. `(mdc x 13)`—e
# instruções especiais—ex. `(define x 13)`, que corresponde à
# instrução de atribuição em Python: `x = 13`.
#
# Essa notação prefixa com parêntesis é chamada _S-expression_ ou _expressão-S_.
# É a mesma notação usada em Lisp e outros dialetos, como Clojure.
#
# Eis um exemplo simples em Scheme, para calcular o MDC (Máximo Divisor Comum):

# %%
# %%lispy

(define (resto m n)
    (- m (* n (quotient m n))))

(define (mdc m n)
    (if (= n 0)
        m
        (mdc n (resto m n))))

(mdc 18 45)


# %% [markdown]
# O mesmo algoritmo em Python:

# %%
def resto(m, n):
    return m - (m // n * n)

def mdc(m, n):
    if n == 0:
        return m
    else:
        return mdc(n, resto(m, n))

mdc(18, 45)

# %% [markdown]
# Assim como temos o operador `%` em Python, Scheme tem a função `modulo`.
# Implementei `resto` apenas para mostrar duas definições de funções.
#
# Em Python é mais eficiente usar um laço `while` do que recursão,
# mas eu queria que os exemplos ficassem parecidos para
# ajudar você a entender o código em Scheme.
# Scheme não tem estruturas de laço como `while` ou `for`.
# Para repetir operações, usa-se recursão.
#
# Note como não há atribuições de variáveis nos exemplos em Scheme e Python acima.
# Muita recursão e pouca atribuição
# são características de programação em um estilo funcional.
#
# O código completo de *lis.py* para Python 3.10 incluindo testes você pode encontrar no diretório
# [18-with-match/lispy/py3.10/](https://github.com/fluentpython/example-code-2e/tree/master/18-with-match/lispy/py3.10/)
# do repositório [fluentpython/example-code-2e](https://github.com/fluentpython/example-code-2e).
#
# O código abaixo apenas confirma a versão do interpretador que está executando este Jupyter Notebook.

# %%
import sys
ver, sub = (3, 10)
assert sys.version_info >= (ver, sub), f'Necessário Python ≥ {ver}.{sub}; instalado: {sys.version}'
sys.version


# %% [markdown]
# ## Imports e tipos
#
# O código escrito pelo Norvig não usa anotações de tipo. Adicionei as anotações e fiz mais algumas pequenas mudanças.


# %%
################ lis.py: Scheme Interpreter in Python
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
## Type hints and minor additions by Luciano Ramalho

import math
import operator as op
from collections import ChainMap
from itertools import chain
from typing import Any, NoReturn, TypeAlias
from collections import abc

Symbol: TypeAlias = str
Atom = float | int| Symbol
Expression = Atom | list

Environment = abc.MutableMapping[Symbol, object]


# %% [markdown]
# Os tipos são definidos são:
#
# `Symbol`: Apenas um apelido para o tipo `str`.
# Instâncias de `Symbol` são usadas como identificadores.
# Nessa versão básica _lis.py_ não há um tipo de dado string com operaçoẽs como
# fatiamento, particionamento (split), conversão de maiúsculas para minúsculas, etc.
#
# `Atom`: Elemento sintático simples: um número ou um `Symbol`.
# Um átomo é o contrário de uma estrutura composta de diversas partes como uma lista.
#
# `Expression`: Programas em Scheme são formados por expressões feitas com átomos e listas, possivelmente aninhadas.
#
# > **NOTA**: O outro interpretador escrito por Norvig,
# [*lispy.py*](https://github.com/fluentpython/example-code-2e/blob/master/18-with-match/lispy/original/lispy.py),
# suporta string como um tipo de dado, assim como também aceita funcionalidades avançadas como macros de sintaxe,
# chamadas de cauda eficientes e _continuations_.
# No entanto, *lispy.py* é quase três vezes mais longo do que *lis.py*, e mais difícil de entender.

# %% [markdown]
# ## O parser
#
# O parser de Norvig são 36 linhas de código que demonstram o poder de Python
# aplicado ao tratamento da sintaxes recursivas de expressões-S.

# %%
def parse(program: str) -> Expression:
    "Read a Scheme expression from a string."
    return read_from_tokens(tokenize(program))

def tokenize(s: str) -> list[str]:
    "Convert a string into a list of tokens."
    return s.replace('(', ' ( ').replace(')', ' ) ').split()

def read_from_tokens(tokens: list[str]) -> Expression:
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


# %% [markdown]
# A função principal desse grupo é `parse`, que toma o código-fonte de uma expressão-S como uma `str`
# e devolve um objeto `Expression`: um `Atom` ou `list` que pode conter mais átomos e listas aninhadas.
#
# O primeiro estágio de um parser é a análise léxica:
# identificar e separar as "palavras" da linguagem,
# conhecidas tecnicamente como _tokens_ ou _itens léxicos_.
# Isso é feito pela função `tokenize`.
#
# Norvig usa um truque esperto em `tokenize`:
# ele coloca espaços antes e depois de cada parênteses no código-fonte, e depois quebra com `.split()`,
# resultando em uma lista de tokens com `(` e `)`
# como itens distintos. Por exemplo, `(f 1)` é transformada em uma lista com quatro itens: `['(', 'f', '1', ')']`.
#
# Esse truque funciona porque o dialeto Scheme simplificado de *lis.py*
# não tem um tipo string, cometários, macros,
# e outros recursos de Scheme padrão que complicam a análise léxica.
# Com essa simplificação, sabemos que todo `(` ou `)` é um delimitador de expressão em *lis.py*.
# (Norvig implementou esses recursos em *lispy.py*.)
#
# O resultado da análise léxica alimenta a análise sintática em `read_from_tokens`,
# que recebe uma lista de itens léxicos e devolve uma `Expression`.
#
# A regras de análise sintática (_parsing_) para esse subconjunto do Scheme são simples:
#
# 1. Um token que se parece com um número é convertido em `float` ou `int`.
# 2. Qualquer outra coisa que não for `(` ou `)` é entendida como `Symbol`—uma `str` a ser usada como identificador.
# Isso inclui código fonte como `+`, `set!` e `make-counter` que são identificadores válidos em Scheme mas não em Python.
# 3. Expressões dentro de `(` e `)` são recursivamente parseadas como
# listas contendo atoms ou listas aninhadas que podem conter atoms e mais listas aninhadas.
#
# Em uma primeira leitura, vale a pena considerar `read_from_tokens` como uma caixa preta,
# e se concentrar na função de alto nível `parse`. Veja alguns exemplos com `parse`:
#
# > **DICA**: Para executar o código em cada uma das células e selecionar a próxima, use `【SHIFT】【ENTER】`.<br>
# Se acontecer `NameError: name 'parse' is not defined`,
# use o comando ***Cell > Run All Above*** do menu para executar as células acima,
# incluindo aquela onde está a definição da função `parse`.

# %%
parse('1.5')

# %%
parse('ni!')

# %%
parse('''
  (define double
    (lambda (n)
      (* n 2)))
''')

# %% [markdown]
# Usando a terminologia de teoria de linguagens de programação
# o objeto `Expression` produzido por `parse` é uma
# **AST** (*Abstract Syntax Tree* ou *Árvore Sintática Abstrata*):
# uma representação conveniente do programa Scheme na forma de
# objetos Python aninhados formando uma estrutura em de árvore,
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
# A árvore acima corresponde à AST `['define', 'double', ['lambda', ['n'], ['*', 'n', 2]]]`.<br>
# Note que todos os elementos da AST são objetos da linguagem Python.
#
# ### Exercício 0
#
# Substitua as reticências `...` com a AST correspondente à cada expressão-S, para obter o resultado `True`.
# Para rodar o código na célula, tecle `【CTRL】【ENTER】`.

# %%
parse('9') == ...

# %%
parse('x/y') == ...

# %%
parse('(+ 3 7)') == ...

# %%
parse('(* c (/ 9 5))') == ...

# %%
parse('(+ 32 (* (/ 9 5) c ))') == ...


# %% [markdown]
# ## Ambiente básico para aritmética
#
# Qualquer linguagem útil precisa fornecer funções pré-definidas, prontas para usar, como
# o módulo `__builtins__` de Python.
#
# Em *lis.py*, a função `standard_env()` constrói e devolve um `Environment` carregado
# com funções pré-definidas.
# Por enquanto usaremos uma versão simplificada, adepois colocaremos mais funções.

# %%
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
            'modulo': op.mod,
            'round': round,
            'symbol?': lambda x: isinstance(x, Symbol),
        }
    )
    return env


# %% [markdown]
# O mapeamento `env` é carregado com:
#
# * todas as funções do módulo `math` do Python;
# * operadores selecionados do módulo `op` do Python;
# * simples mas poderosas funções construídas com `lambda` do Python;
# * built-ins do Python renomeados, por exemplo `callable` como `procedure?` ou diretamente mapeados como `round`.

# %% [markdown]
# ## Uma calculadora
#
# A primeira versão da função `evaluate` trata expressões com funções embutidas e váriavies definidas pela usuária.
#
# > **NOTA**: o parser de Norvig é simples e sólido, mas seu avaliador é simples e frágil. Ele omitiu a verificação de erros para manter a lógica simples de ser acompanhada. Nas palavras dele: "Lispy não tenta detectar, reportar razoavelmente, ou se recuperar de erros. Lispy espera que o programador seja perfeito." ([fonte](https://norvig.com/lispy.html)).

# %%
def evaluate(exp: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    match exp:
        case Symbol(name):                                  # variable reference
            return env[name]
        case int(x) | float(x):                             # number literal
            return x
        case ['define', Symbol(var), value_exp]:            # (define var exp)
            env[var] = evaluate(value_exp, env)
        case [Symbol(name), *args]:                         # (proc arg...)
            proc = env[name]
            values = (evaluate(arg, env) for arg in args)
            return proc(*values)
        case _:
            raise SyntaxError(lispstr(exp))


# %% [markdown]
# Execute esses exemplos para ver o `evaluate` em ação.
#
# Um quadrado curioso:

# %%
evaluate(parse('(* 11111 11111)'), standard_env())

# %% [markdown]
# Se existem 700 candidatas e 294 foram aprovadas, que porcentagem foi aprovada?

# %%
evaluate(parse('(* (/ 294 700) 100)'), standard_env())

# %% [markdown]
# Agora vamos estudar cada `case` do `match` em `evaluate`.

# %% [markdown]
# ### Avaliar símbolo
#
# ```python
#         case Symbol(name):                                  # variable reference
#             return env[name]
# ```
#
# Se a expressão é um `Symbol`, então obtenha seu valor no ambiente.

# %%
evaluate('pi', standard_env())

# %%
evaluate('+', standard_env())

# %% [markdown]
# ### Avaliar números
#
# ```python
#         case int(x) | float(x):                             # number literal
#             return x
# ```
#
# Se a expressão é um literal `int` ou `float`, devolva o valor do literal.

# %%
evaluate(1.5, standard_env())

# %% [markdown]
# ### Evaluate `(define var exp)`
#
# ```python
#         case ['define', Symbol(var), value_exp]:            # (define var exp)
#             env[var] = evaluate(value_exp, env)
# ```
#
# Se a expressão é uma lista de três elementos começando com a palavra reservada `define`,
# seguida por um `Symbol` e uma `Expression`,
# então avalie recursivamente a expressão no ambiente e
# armazene o resultado em `env` usando o `Symbol` como chave.

# %%
env = standard_env()
evaluate(parse('(define answer (/ 378 9))'), env)
env['answer']

# %% [markdown]
# ### Avaliar chamada de função `(proc arg…)`
#
# ```python
#         case [Symbol(name), *args]:                         # (proc arg...)
#             proc = env[name]
#             values = (evaluate(arg, env) for arg in args)
#             return proc(*values)
# ```
#
# Se a expressão é uma `list` começando com um `Symbol`, seguida de 0 ou mais itens:
#
# 1. Obtenha a função no ambiente. Em Scheme, funções são chamadas "procedures", daí `proc`.
# 2. Avalie as expressões restantes (os valores dos argumentos).
# 3. Invoque o procedimento passando os valores dos argumentos.
#
# > **NOTA**: quando implementarmos `lambda`, o primeiro item desse caso poderá ser qualquer expressão que devolva uma função, e não apenas um `Symbol`.

# %%
evaluate(['quotient', 11, 4], standard_env())

# %%
evaluate(['*', ['/', 123, 876], 100], standard_env())

# %% [markdown]
# `evaluate()` consegue processar expressões profundamentes aninhadas, mas somente uma expressão no nível mais alto.
# Para agrupar várias expressões, use a função `(begin...)`.
# A função `evaluate()` avalia todos os argumentos antes de invocar `begin`, e `begin` simplesmente devolve o valor do último argumento passado.
# Por exemplo:

# %%
env = standard_env()
percent = """
(begin
  (define a 126)
  (define b (* 6 50))
  (* (/ a b) 100)
)
"""
evaluate(parse(percent), env)

# %% [markdown]
# Após rodar o código acima, as variáveis `a` e `b` agora estão definidas no ambiente `env`:

# %%
env['a'], env['b']


# %% [markdown]
# Para avaliar um programa inteiro, use a função `run()`, descrita a seguir.

# %% [markdown]
# ## Execução de programas formados por várias expressões-S
#
# As funções a seguir aceitam o código fonte de um programa em Scheme
# como uma string formada por uma sequência de expressões-S, e as executam em ordem.

# %%
def run_lines(source: str, env: Environment|None = None) -> abc.Iterator:
    global_env: Environment = ChainMap({}, standard_env())
    if env is not None:
        global_env.update(env)
    tokens = tokenize(source)
    while tokens:
        exp = read_from_tokens(tokens)
        yield evaluate(exp, global_env)


def run(source: str, env: Environment|None = None) -> Any:
    result = None
    for result in run_lines(source, env):
        pass
    return result


# %% [markdown]
# Com `run()` não precisamos de `(begin …)` para avaliar múltiplas expressões de uma vez só.
#
# Mas `begin` ainda é necessário para definir funções "impuras" que podem conter múltiplas expressões.

# %%
porcento = """
(define a 126)
(define b (* 6 50))
(* (/ a b) 100)
"""
run(porcento)

# %% [markdown]
# ## Integração com Jupyter Notebook

# %%
import sys

from IPython.core.magic import register_cell_magic

@register_cell_magic
def mylispy(line, cell):
    """Evaluate cell."""
    return run(cell)


# %%
# %%mylispy a
(+ 1 1)

# %% [markdown]
# ### Exercício 1
#
# Essa é a fórmula para converter temperaturas de Celsius para Fahrenheit:
#
# `f = (9 / 5) * c + 32`
#
# Eis o código para converter 20°C para °F:

# %%
# %%mylispy

(define c 20)
(+ 32 (* c (/ 9 5)))

# %% [markdown]
# A função inversa é:
#
# `c = (f − 32) * 5 / 9`
#
# No código abaixo, substitua `(+ 1 2)` pela expressão correta para converter 68°F para °C.

# %%
# %%mylispy

(define f 68)
(+ 1 2)

# %% [markdown]
# ### Exercicio 2
#
# O módulo `math` de Python inclui uma função `factorial`, que é incluída ambiente criado por `standard_env()`:

# %%
# %%mylispy

(factorial 10)

# %% [markdown]
# A linguagem Scheme aceita `!` como um identificador. Sua tarefa é fazer `factorial()` de Python ficar disponível através do símbolo `!` em Scheme.
#
# **Passo 2.1** Descomente a expressão abaixo e execute para ver o erro. Veja para a última linha da saída do erro.
# Qual é o erro? Você entende porque esse é o erro?

# %%
# run('(! 10)') == 3628800

# %% [markdown]
# **Passo 2.2.** Edite a função `standard_env` acima para adicionar uma entrada para `'!'`,
# mapeando para a função `math.factorial` de Python.
#
# **Passo 2.3.** Execute a expressão acima para confirmar que o resultado é `True`.

# %% [markdown]
# ### Exercício 3
#
# Em um Scheme padrão, o operador `+` é variádico, ou seja, aceita qualquer quantidade de argumentos.
# Com 0 argumentos, `(+)` retorna `0`; caso contrário retorna a soma de todos os argumentos.
#
# **Step 3.1.** Descomente a expressão abaixo e execute para ver o erro. O erro faz sentido para você?

# %%
# run('(= 10 (+ 1 2 3 4))')

# %% [markdown]
# **Passo 3.2.** Edite a função `standard_env` acima para reimplementar o `+`,
# fazendo a expressão acima devolver `True`.
#
# > **DICA 3.2.1**: Escondida abaixo está uma versão variádica do `sum` do Python.
# Tente resolver o exercício sem revelar a dica.
# Para revelar o código, descomente a linha do `print()` e execute a célula.
#

# %%
from base64 import b64decode
blob = (b'ZGVmIHZhcmlhZGljX3N1bSgqYXJncyk6CiAgICByZXR1cm4g'
        b'c3VtKGFyZ3MpCgp2YXJpYWRpY19zdW0oMSwgMiwgMywgNCk=')
# print(b64decode(blob).decode('utf8'))

# %% [markdown]
# > **DICA 3.2.2**: A seguir, a mesma função em uma única linha do Python.
# Tente resolver o exercício sem revelar a dica.
# Para revelar, descomente a linha do `print()` e execute a célula.

# %%
blob = b'ZiA9IGxhbWJkYSAqYXJnczogc3VtKGFyZ3MpCmYoMSwgMiwgMywgNCk='
# print(b64decode(blob).decode('utf8'))

# %% [markdown]
# ## Um ambiente mais completo

# %% [markdown]
# Esta nova versão de `standard_env()` define funções para trabalhar com listas.

# %%
import time

def now():
    """Returns local time as ['HH', 'MM', 'SS']"""
    time_parts = list(time.localtime())[3:6]
    return [f'{n:02d}' for n in time_parts]

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
            'display': lambda x: print(lispstr(x), end=''),
            'newline': lambda: print(),
            'return': lambda: print(end='\r', flush=True),
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
            'now': now,
            'null?': lambda x: x == [],
            'number?': lambda x: isinstance(x, (int, float)),
            'procedure?': callable,
            'round': round,
            'sleep': time.sleep,
            'symbol?': lambda x: isinstance(x, Symbol),
        }
    )
    return env

# %% [markdown]
# A função `(display …)` exibe resultados, mas não emite uma quebra de linha.
# Para isso é preciso chamar `(newline)` (assim mesmo, sem argumentos).
#
# A chamada `(display x)` invoca `lispstr(x)`, que faz o inverso de `parse()`.
# Dado um objeto Python que representa uma expressão,
# `lispstr` devolve o código-fonte em Scheme para essa expressão.

# %%
def lispstr(exp: Expression) -> str:
    "Convert a Python object back into a Lisp-readable string."
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')'
    else:
        return str(exp)

# %% [markdown]
# Por exemplo:

# %%
lispstr(['+', 32, ['*', ['/', 9, 5], 'c']])

# %% [markdown]
# ## `Procedure`: uma classe que representa uma _closure_
#
# A próxima melhoria do `evaluate()` será incluir `(lambda …)` para permitir que
# usuários definam funções, ou _procedimentos_ no jargão de Scheme.
# Para implementar `lambda`, precisamos de uma classe que represente um procedimento.


# %%
class Procedure:
    """A user-defined Scheme procedure."""

    def __init__(self, parms: list[Symbol], body: Expression, env: Environment) -> None:
        self.parms = parms
        self.body = body
        self.env = env

    def __call__(self, *args: Expression) -> Any:
        local_env = dict(zip(self.parms, args))
        env: Environment = ChainMap(local_env, self.env)
        return evaluate(self.body, env)


# %% [markdown]
# A classe `Procedure` poderia muito bem ser nomeada `Closure`,
# porque é isso o que ela representa:<br>
# uma definição de função junto com um ambiente capturado quando a função é definida.
#
# Os parâmetros obrigatórios para criar uma `Procedure` são:
#
# `parms`: uma lista de símbolos que representam os nomes dos parâmetros da função.
# A lista pode estar vazia.
#
# `body`: o corpo da função como uma expressão que será interpretada quando a função for chamada.
#
# `env`: o ambiente onde a função é criada. Isso é o que torna o procedimento uma
# [*closure*](https://en.wikipedia.org/wiki/Closure_(computer_programming)) ou
# [*clausura*](https://pt.wikipedia.org/wiki/Clausura_(ci%C3%AAncia_da_computa%C3%A7%C3%A3o))
# (o termo em PT não é muito usado, mas o
# [artigo na Wikipédia](https://pt.wikipedia.org/wiki/Clausura_(ci%C3%AAncia_da_computa%C3%A7%C3%A3o)) é bom).
#
# O método `__init__` apenas guarda os argumentos recebidos. Nenhum deles é avaliado quando a função é definida.
#
# O ambiente `self.env` é usado quando a função é chamada para fornecer os valores de
# [variáveis não-locais](https://en.wikipedia.org/wiki/Non-local_variable):<br>
# variáveis que aparecem no corpo da função mas que não são parâmetros ou variáveis locais.
#
# Vamos criar uma `Procedure` "na mão" para ver como funciona:

# %%
dobro = Procedure(['n'], ['*', 'n', 2], standard_env())
dobro(4)


# %% [markdown]
# ## Avaliador com `lambda`, `if` e `quote`
#
# Para transformar a calculadora em um subconjunto digno da linguagem Scheme,
# precisamos permitir funções definidas pelo usuário,
# condicionais e a instrução `(quote …)` para lidar com expressões
# como dados ao invés de interpretá-las.
#

# %%
def evaluate(exp: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    match exp:
        case int(x) | float(x):                             # number literal
            return x
        case Symbol(var):                                   # variable reference
            return env[var]
        case ['quote', exp]:                                # (quote exp)
            return exp
        case ['if', test, consequence, alternative]:        # (if test consequence alternative)
            if evaluate(test, env):
                return evaluate(consequence, env)
            else:
                return evaluate(alternative, env)
        case ['define', Symbol(var), value_exp]:            # (define var exp)
            env[var] = evaluate(value_exp, env)
        case ['lambda', [*parms], body]:                    # (lambda (parm...) body)
            return Procedure(parms, body, env)
        case [op, *args]:                                   # (proc arg...)
            proc = evaluate(op, env)                        # here proc can be a function expression
            values = (evaluate(arg, env) for arg in args)
            return proc(*values)
        case _:
            raise SyntaxError(lispstr(exp))



# %% [markdown]
# ### Avaliar `(lambda (var…) body)`
#
# ```python
#         case ['lambda', [*parms], body]:  # (lambda (parm...) body)
#             return Procedure(parms, body, env)
# ```
#
# Se a expressão é uma lista onde o primeiro elemento é a
# palavra reservada `lambda`,
# seguida por uma lista de zero ou mais símbolos,
# e finalmente um `body` formado por uma única expressão,
# então criamos e devolvemos um objeto `Procedure`.
#
# Exemplo:

# %%
porcentagem = run('(lambda (a b) (* (/ a b) 100))')
porcentagem

# %%
porcentagem(15, 20)

# %% [markdown]
# O resultado de `(lambda …)` é uma função anônima,
# ela não é salva no ambiente. Para criar uma função nomeada,
# use `lambda` com `define`.
#
# > **NOTA**: Essa versão de *lis.py* só aceita uma única expressão como corpo de uma função.
#   Use `(begin …)` para criar um `body` várias expressões. O resultado da função será o valor da última expressão.

# %% [markdown]
# ### Avaliar `(quote exp)`
#
# ```python
#         case ['quote', exp]:                                # (quote exp)
#             return exp
# ```
#
# Se a expressão for uma `list` onde o primeiro elemento
# é a palavra reservada `quote` seguida por uma única expressão,
# devolva a expressão sem interpretá-la.
#
# A instrução `quote` permite construir uma lista a partir de uma expressão-S.
#
# Exemplos:

# %%
run('(quote no-such-name)')  # símbolo não definido, iria causar um erro se fosse interpretado

# %%
run('(quote (99 bottles of beer))')  # 99 não é o nome de uma função ou palavra reservada

# %%
run('(quote (/ 10 0))')  # se interpretada, a expressão causaria uma exceção de divisão por zero

# %% [markdown]
# ### Avaliar `(if test consequence alternative)`
#
# ```python
#         case ['if', test, consequence, alternative]:        # (if test consequence alternative)
#             if evaluate(test, env):
#                 return evaluate(consequence, env)
#             else:
#                 return evaluate(alternative, env)
# ```
#
# Se a expressão é uma `list` onde o primeiro elemento é a palavra reservada `if`,
# seguida por exatamente três expressões, avalie a expressão `test`.
# Se o resultado é `True`, avalie `consequence` devolva seu valor;
# caso contrário, avalie `alternative` e devolva seu valor.
#
# Exemplos:

# %%
run('(if (= 3 3) 1 0)')

# %%
run('(if (= 3 30) 1 0)')

# %%
# %%mylispy

(define avaliar
    (lambda (nota)
        (if (>= nota 5)
            (quote PASSOU)
            (quote NÃO-PASSOU))))
(avaliar 7)

# %% [markdown]
# ## Exemplos

# %% [markdown]
# ### Máximo divisor comum
#
# O [algoritmo de Euclides](https://pt.wikipedia.org/wiki/Algoritmo_de_Euclides).

# %%
# %%mylispy

(define resto (lambda (m n)
    (- m (* n (quotient m n)))))

(define mdc (lambda (m n)
    (if (= n 0)
        m
        (mdc n (resto m n)))))

(mdc 18 45)


# %% [markdown]
# > **NOTA**: Este exemplo usa `lambda` dentro de `define` em vez da sintaxe abreviada com `define`
# ilustrada na função `mdc` em [Sintaxe de Scheme](#Sintaxe-de-Scheme).
# Leia mais sobre essa sintaxe abreviada em [Açúcar Sintático](Açúcar-Sintático).

# %% [markdown]
# ### Relógio

# %%
# %%mylispy

(display (now))

# %%
# %%mylispy

(define clock (lambda ()
    (begin
        (display (car (now)))
        (display (quote :))
        (display (car (cdr (now))))
        (display (quote :))
        (display (car (cdr (cdr (now)))))
        (return)
        (sleep 1)
        (clock))))

(if 0 (clock) 0)

# %% [markdown]
# ### Fatorial recursivo simples

# %%
# %%mylispy

(define ! (lambda (n)
    (if (< n 2)
        1
        (* n (! (- n 1)))
)))

(! 5)

# %% [markdown]
# ### Fatorial com recursão de cauda

# %%
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

# %% [markdown]
# Na função `fatorial-iter`,
# a chamada recursiva é devolvida como resultado diretamente.
# Isso é denominado uma _recursão de cauda_,
# um caso particular de _chamada de cauda_ ou [*tail call*](https://en.wikipedia.org/wiki/Tail_call),
# quando a última operação no caminho de execução de uma função é
# devolver o resultado de invocar outra função (ou própria, no caso de recursão).
#
# Em contraste, o [Fatorial recursivo simples](#Fatorial-recursivo-simples)
# não é um exemplo de recursão de cauda:
# o resultado da chamada recursiva é multiplicado por `n` antes de ser devolvido.
#
# O sufixo `-iter` é comumente usado em Scheme para funções que fazem iteração por recursão de cauda.
# É comum que tais funções utilizem um parâmetro acumulador,
# que vai gradualmente acumulando resultados parciais.
# Em `fatorial-iter`, o parâmetro `produto` é o acumulador.
#
# > **NOTA**: *lis.py* não implementa chamadas de cauda eficientes, um recurso conhecido em inglês como _proper tail call_ (PTC) ou _tail call optimization_ (TCO), conforme o autor.
# Portanto, não há vantagem em fazer recursão de cauda. Porém
# [*lispy.py*](https://github.com/norvig/pytudes/blob/main/py/lispy.py) e
# [`mylis_2`](https://github.com/fluentpython/lispy/blob/main/mylis/mylis_2/lis.py)
# implementam PTC, o que significa que nesses interpretadores uma recursão de cauda não faz a pilha crescer a cada iteração.

# %% [markdown]
# ### Raiz quadrada por aproximação
#
# O [método babilônio](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method),
# um algoritmo para calcular raiz quadrada usado desde a antiguidade.
# É um caso especial do
# [Método de Newton–Raphson](https://pt.wikipedia.org/wiki/M%C3%A9todo_de_Newton%E2%80%93Raphson)
# para calcular raízes de funções.
# Adaptei o código abaixo de um
# [exemplo](https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node12.html)
# do livro clássico *Structure and Interpretation of Computer Programs*.

# %%
# %%mylispy

(define raiz2 (lambda (x)
    (raiz2-iter 1.0 x)))

(define raiz2-iter (lambda (chute x)
    (if (aproximado? chute x)
        chute
        (raiz2-iter (melhorar chute x) x))))

(define aproximado? (lambda (chute x)
    (< (abs (- (* chute chute) x)) 0.001)))

(define melhorar (lambda (chute x)
    (média chute (/ x chute))))

(define média (lambda (x y)
    (/ (+ x y) 2)))

(raiz2 12345654321)

# %% [markdown]
# ### Média de uma lista de números
#
# Um exemplo simples com operações de manipulação de lista: `null?`, `car`, `cdr`, `list`.

# %%
# %%mylispy

(define média (lambda (lista)
    (média-iter lista 0 0)))

(define média-iter (lambda (lista contador soma)
    (if (null? lista)
        (/ soma contador)
        (média-iter (cdr lista) (+ contador 1) (+ soma (car lista))))))

(média (list 1 2 3 4))

# %% [markdown]
# ### Quicksort
#
# O [algoritmo recursivo de ordenação](https://pt.wikipedia.org/wiki/Quicksort) eficiente e elegante inventado por Tony Hoare.
#
# Note as funções `append` e `filter` que operam com listas.
# Confira em `standard_env()` que
# o comportamento de `append` em Scheme é criar uma nova lista concatenando listas.
# É mais parecido com `l1 + l2` em Python, e bem diferente de fazer `l1.append(l2)`.

# %%
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

(display (quicksort (list 2 1 6 3 4 0 8 9 7 5)))
(newline)
"""
run(quicksort_scm)

# %% [markdown]
# > **NOTA**: o código acima funciona em *lis.py*, mas não em Scheme padrão.
# A forma `define` não pode ser usada naquele contexto em Scheme padrão;
# em vez dela, usaríamos a forma `let`—que não existe em *lis.py*.
# O funcionamento de `define` em *lis.py* é uma simplificação feita por Norvig,
# imitando a semântica de atribuições em Python.
# Quando `define` ocorre no nível global, é criada ou atualizada uma variável no ambiente global.
# No corpo de uma função, `define` cria ou atualiza uma variável
# no primeiro dicionário do `ChainMap` que define o ambiente local.

# %% [markdown]
# ## Açúcar Sintático
#
# Por padrão, Scheme tem uma sintaxe alternativa para `define` que permite definir funções nomeadas
# sem usar a palavra reservada `lambda`.
#
# A sintaxe é: `(define (name params…) body…)`, onde:
#
# `name`: é o nome da função a ser definida (um `Symbol`);
#
# `params…`: zero ou mais símbolos declarando o nome dos parâmetros;
#
# `body…`: uma ou mais expressões para serem usadas como o corpo da função.
#
# Isso é um exemplo de _açúcar sintático_:
# uma sintaxe nova que não adiciona nenhuma funcionalidade à linguagem,
# mas facilita o uso dela, tornando-a mais conveniente.
#
# A versão de `mdc` apresentada na seção [Sintaxe de Scheme](#Sintaxe-de-Scheme)
# usa esse atalho sintático.
#
# ### Trabalho de conclusão
#
# Valide seu entendimento do *lis.py* modificando a função `evaluate()`
# para permitir o atalho sintático `define` para funções nomeadas.<br>
# Teste sua solução rodando o exemplo abaixo. O resultado deve ser `9`.
#

# %%
mdc2_scm = '''
(define (resto m n)
    (- m (* n (quotient m n))))

(define (mdc m n)
    (if (= n 0)
        m
        (mdc n (resto m n))))

(mdc 18 45)
'''
run(mdc2_scm)

# %% [markdown]
# ## Fim

# %% [markdown]
# Parabéns por ter chegado até aqui!
#
# A célula a seguir torna mais fácil verificar que nenhuma célula acima está levantando uma exceção.

# %%
run('(display (quote (Este é o lis.py de Norvig))) (newline)')


# %% [markdown]
# # Material Extra

# %% [markdown]
# ## O REPL
#
# O REPL (Read-Eval-Print-Loop) de Norvig é fácil de entender porém não é
# muito amigavél.
# A função `repl()` exibe um prompt `lis.py>`,
# e nele devemos digitar expressões corretas e completas;
# se esquecermos de fechar um parêntesis o _lis.py_ irá quebrar.
#
# > **DICA**: O `repl()` não funciona bem dentro do Jupyter Notebook.
#   É melhor experimentá-lo executando o arquivo _lispy.py_ como um script:<br>
#   `$ python3 norvigs-lispy.py`

# %%
def repl(prompt: str = 'lis.py> ') -> NoReturn:
    "A prompt-read-evaluate-print loop."
    global_env: Environment = ChainMap({}, standard_env())
    while True:
        val = evaluate(parse(input(prompt)), global_env)
        if val is not None:
            print(lispstr(val))

if __name__ == '__main__' and '__file__' in globals():
    import readline  # activate readline
    repl()

# %% [markdown]
# A função `repl()` chama a função `standard_env()` para instalar funções essenciais no ambiente global,
# então entra em um loop infinito lendo e fazendo o parse de cada linha digitada pelo usuário,
# interpretando cada linha no ambiente global, e mostrando o resultado—exceto quando é `None`.
# A variável `global_env` pode ser modificada pela função `evaluate()`
# quando se usa `(define …)` no escopo global.

# %% [markdown]
# > **NOTA**: Quando estudei os scripts _lis.py_ e _lispy.py_ feitos por Norvig,
#   criei um fork chamado [`mylis`](https://github.com/fluentpython/lispy/blob/main/mylis)
#   com algumas funcionalidades a mais,
#   como um REPL que aceita expressões parciais que podem ser continuadas nas próximas linhas,
#   semelhante ao REPL de Python que sabe que não terminamos e nos apresenta um segundo prompt
#   `...` até que entremos um expressão ou instrução completa que possa ser interpretada.
#   `mylis` também consegue tratar alguns erros um pouco melhor, porém ainda é fácil de quebrar.

