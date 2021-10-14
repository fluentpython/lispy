# Bem-vindas ao guia de contribuição do `Tutorial de lis.py - Python Brasil 2021` <!-- omit in toc -->

Grato por dedicar seu tempo em contribuir para nosso projeto!  :sparkles:.

Leia nosso [Código de Conduta](https://python.org.br/cdc/) para manter nossa comunidade acessível e respeitosa.


## Antes de começar

- Esse material é uma versão traduzida da oficina sobre [lis.py](https://github.com/fluentpython/lispy/tree/main/workshops/thoughtworks2021) para Português do Brasil.
- Para facilitar a visualização das mudanças realizadas nos notebooks (arquivos `.ipynb`)
estamos utilizando a ferramenta [`jupytext`](https://github.com/mwouts/jupytext).
- Estamos usando o Python 3.7 ([link](https://www.python.org/downloads/release/python-379/))
para manter a compatibilidade com a versão mais recente do https://mybinder.org/[Binder] disponível em outubro de 2021. O Binder permite que as participantes da oficina possam interagir com o material e fazer exercícios sem usando apenas um navegador, sem precisar instalar Python ou Jupyter Notebook.


## Reportando bugs/typos e sugerindo melhorias

Você é mais que bem-vinda a sugerir melhorias ao tutorial.
Pedimos apenas que que verifique se nenhum
[issue](https://github.com/fluentpython/lispy/issues) ou
[pull request](https://github.com/fluentpython/lispy/pulls)
já tenha sido criado por outra pessoa com a mesma sugestão.
Em caso afirmativo, veja se pode contribuir com ela.

> **DICA:** Caso decida trabalhar em alguma _issue_,
indique sua intenção em um comentário na _issue_ escolhida.
Dessa forma, outras pessoas saberão que tem alguém trabalhando nela.
Caso tenha ficado perdido ou com dúvidas, peça ajuda. <!-- como? -->

Caso seja algo novo, crie uma nova [issue](https://github.com/fluentpython/lispy/issues).

O mesmo vale para typos (erros ortograficos).

## Contribuindo

Você decidiu contribuir para o projeto! Yay!

- Faça um fork do projeto, clone o repositorio e crie um novo branch.
Mais detalhes [aqui](https://docs.github.com/pt/enterprise/2.17/user/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork).

- Sugerimos que crie um ambiente virtual (Python 3.7) para poder instalar todas as dependencias do projeto.

- Faça as modificações desejadas no arquivo `norvigs-lispy.py`.

- Gere uma cópia local do Jupyter Notebook `norvigs-lispy.ipynb` através deste comando:

```
$ jupytext --to notebook norvigs-lispy.py
```

- Confira seu trabalho vizualizando `norvigs-lispy.ipynb` no navegador:

```
$ jupyter notebook norvigs-lispy.ipynb
```

- Caso faça alterações no Jupyter Notebook,
execute o comando a seguir para atualizar o arquivo `norvigs-lispy.py`:

```
$ jupytext --to py norvigs-lispy.ipynb
```

> **IMPORTANTE:** Não esqueça de atualizar o `norvigs-lispy.py` sempre que fizer alterações no Jupyter Notebook,
porque o arquivo `norvigs-lispy.ipynb` não vai para o repositório!
O motivo é que `git`, `Github` e ferramentas auxiliares não conseguem mostrar claramente as diferenças entre arquivos `.ipynb`,
o que dificulta a revisão das alterações feitas.

Sentiu falta de algo nesse guia? Conta pra gente! <!-- como? -->
