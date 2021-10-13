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
https://github.com/fluentpython/lispy/issues[issue] ou
https://github.com/fluentpython/lispy/pulls[pull request]
já tenha sido criado por outra pessoa com a mesma sugestão.
Em caso afirmativo, veja se pode contribuir com ela.

Caso seja algo novo, crie uma nova
https://github.com/fluentpython/lispy/issues[issue].

O mesmo vale para typos (erros ortograficos).

## Contribuindo

Você decidiu contribuir para o projeto! Yay!

- Faça um fork do projeto, clone o repositorio e crie um novo branch.
Mais detalhes [aqui](https://docs.github.com/pt/enterprise/2.17/user/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork).

- Sugerimos que crie um ambiente virtual (Python 3.7) para poder instalar todas as dependencias do projeto.

- Faça as modificações desejadas.

- Caso o arquivo modificado seja o Jupyter Notebook `norvigs-lispy.ipynb`,
execute o comando a seguir para gerar o arquivo `.py` correspondente,
que é mais fácil de revisar usando as ferramentas de comparação de código (_diff tools_).

```
$ jupytext --to py norvigs-lispy.ipynb
```

- Se preferir editar o arquivo `norvigs-lispy.py`, execute o comando inverso para sincronizar o notebook:

```
$ jupytext --to notebook norvigs-lispy.py
```

Sentiu falta de algo nesse guia? Conta pra gente! <!-- como? -->

> **DICA:** Caso decida trabalhar em alguma _issue_, comente na _issue_ escolhida.
Dessa forma, outras pessoas saberão que tem alguém trabalhando nela.
Caso tenha ficado perdido ou com dúvidas, peça ajuda. <!-- como? -->
