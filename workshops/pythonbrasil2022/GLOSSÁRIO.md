# GLOSSÁRIO

Termos do domínio de _linguagens de programação_ adotados nesta oficina.


**AST**: Abstract Syntax Tree (Árvore Sintática Abstrata); estrutura de dados que representa o programa
de uma forma conveniente para o trabalho do _interpretador_ ou _compilador_.
Em Python uma AST pode ser construída com listas, dicionários, e/ou objetos aninhados.

**compilador**: programa que lê código-fonte e gera códigos binários para processadores específicos,
na forma de binários executáveis ou bibliotecas.
Compiladores usam _AST_ como representação do programa;
a construção do binário é guiada pela AST.
Ver: _interpretador_.

**infixo**: símbolo que aparece entre duas expressões, por exemplo operadores aritméticos infixos: `a + b`.
Ver: _prefixo_.

**interpretador**: programa que lê código-fonte e o executa em memória,
sem gerar um arquivo binário executável.
Alguns interpretadores executam o programa lendo a AST,
outros usam a AST para gerar _bytecode_ para uma _máquina virtual_.
Ver: _compilador_.

**parser**: função que recebe o código-fonte e devolve uma _AST_ formada por objetos da _linguagem hospedeira_.
Ex: `parse('(* 2 pi)')` resulta em `['*', 2, 'pi']`.
Traduções: _analisador sintático_ ou _analisador_.

**prefixo**: símbolo que antecede uma ou mais expressões, por exemplo o operador aritmético prefixo
`+ a b c d` devolve a soma dos quatro valores.
Em Scheme, expressões prefixas são delimitadas por parêntesis `(+ a b c d)`. Ver: _infixo_.

**prompt**: sinal gráfico que um programa exibe para indicar que está esperando uma entrada.
Por exemplo, o prompt padrão do Python é `>>>`.

**REPL**: REPL ou Read-Eval-Print-Loop é um programa interativo quem exibe um _prompt_
onde digitamos comandos ou expressões e o programa interpreta o que digitamos,
mostra o resultado, e volta para o estado inicial exibindo o prompt e esperando uma nova instrução do usuário.

**varíadico**: um parâmetro de função que aceita zero ou mais argumentos.
Ex.: `def soma(*p): return sum(p)` cria uma função com o parâmetro variádico `p`, podendo ser usada como `soma()`, `soma(1, 2)`, ou `soma(1, 2, 3, 4)`, ou `soma(*range(1_000_000))`.
Em Scheme, alguns operadores como `+`, `*`, `=`, `<=` são variádicos. Por exemplo, `(+ 1 2 3 4)` devolve o valor `10`.
