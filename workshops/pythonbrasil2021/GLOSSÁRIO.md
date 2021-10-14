# GLOSSÁRIO

Termos do domínio de _linguagens de programação_ adotados nesta oficina.


**AST**: Abstract Syntax Tree (Árvore Sintática Abstrata); estrutura de dados que representa o programa
de uma forma conveniente para o trabalho do _interpretador_ ou _compilador_.
Em Python uma AST pode ser construída com listas, dicionários, e/ou objetos aninhados.

**Compilador**: programa que lê código-fonte e gera códigos binários para processadores específicos, na forma de binários executáveis ou bibliotecas. Compiladores usam _AST_ como representação do programa; a construção do binário é guiada pela AST. Ver: _interpretador_.

**Fork**: Chamamos de fork um projeto git que foi derivado de outro projeto git, podemos encarar ele como uma cópia do projeto original onde podemos experimentar mudanças livremente.

**Infixo**: símbolo que aparece entre duas expressões, por exemplo operadores aritméticos infixos: `a + b`. Ver: _prefixo_.

**Interpretador**: programa que lê código-fonte e o executa em memória, sem gerar um binário para a máquina hospedeira. Alguns interpretadores executam o programa lendo a AST, outros utilizam a AST para gerar _bytecode_ para uma _máquina virtual_. Ver: _compilador_.

**Parser**: função que recebe o código-fonte e devolve uma _AST_ formada por objetos da _linguagem hospedeira_.
Ex: `parse('(* 2 pi)')` resulta em `['*', 2, 'pi']`.
Traduções: _analisador sintático_ ou _analisador_.

**Prefixo**: símbolo que antecede uma ou mais expressões, por exemplo o operador aritmético prefixo
`+ a b c d` devolve a soma dos quatro valores.
Em Scheme, expressões prefixas são delimitadas por parêntesis `(+ a b c d)`. Ver: _infixo_.

**Prompt**: É um sinal gráfico que um programa mostra para o usuário para mostrar que está esperando uma entrada, podemos observar isso quando estamos no terminal e fica algo parecido com `|` piscando e aguardando um comando.

**REPL**: REPL ou Read-Eval-Print-Loop é como podemos chamar um shell interativo onde a gente entra algum comando/expressão o programa interpreta aquilo, mostra o resultado e volta para o estado inicial esperando por novas entradas do usuário. Um grande exemplo disso é quando abrimos o shell do Python e ele fica com o prompt. `>>>`, esperando que digitemos alguma coisa.

**Varíadico**: um parâmetro em uma função que aceita zero ou mais argumentos. Ex.: `def soma(*p): return sum(p)` cria uma função com o parâmetro variádico `p`, podendo ser usada como `soma()`, `soma(1, 2)`, ou `soma(1, 2, 3, 4)`, ou `soma(*range(1_000_000))`.
Em Scheme, alguns operadores como `+`, `*`, `=`, `<` são variádicos: `(+ 1 2 3 4)`.
