

# Norvig's lispy: beautiful and illuminating

## Abstract

Why isn't `if` a function?
Why does Python need to add keywords like `yield` or `async` from time to time?
What precisely is a closure, what problem does it solve, and how does it work?
These are some of the fundamental questions you'll be able to answer after this tutorial:
an interactive exploration of Peter Norvig's `lis.py`–an interpreter for
a subset of the Scheme dialect of Lisp written in Python.

## Description

[Peter Norvig](https://norvig.com/) of Stanford University wrote
[`lis.py`](https://github.com/norvig/pytudes/blob/main/py/lis.py):
an interpreter for a subset of the Scheme dialect of Lisp in 132 lines of readable Python.
I took Norvig's beautiful code, updated it to modern Python,
and integrated it into a Jupyter notebook with explanations as well as
interactive experiments and exercises checked automatically.

Why should you study lis.py? This is what I got out of it:

* Learning how an interpreter works gave me a deeper understanding of Python and programming languages in general—interpreted or compiled.

* The simplicity of Scheme is a master class of language design.

* `lis.py` is a beautiful example of idiomatic Python code.

## Audience

This tutorial is for practicing Python programmers who want to learn how a programming language works,
through a beautiful, functional example showcasing modern Python best practices.
Attendees will experiment with the building blocks of an interpreter,
learning fundamental concepts about parsing,
recursive evaluation of expressions and control structures,
representing functions with closures—among other topics.

## Format

This is a 3-hour tutorial including with 30 minutes for exercises and 30 minutes for a break.
The presentation is split in three parts, with exercises after each part.

## Required software

Attendees need a modern Web browser connected to the Internet.

With Norvig's permission, I've updated `lis.py` and integrated it into a
[Jupyter notebook](https://github.com/fluentpython/lispy/blob/main/workshops/thoughtworks2021/norvigs-lispy.ipynb)
so that attendees can study and experiment with each major component as we build the interpreter.
The notebook runs on Binder, so attendees don't need to install anything:
they will do all experimentation and coding exercises using just a Web browser.

Click [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fluentpython/lispy/HEAD?labpath=workshops%2Fthoughtworks2021%2Fnorvigs-lispy.ipynb)
 to run this tutorial's Jupyter notebook at Binder. 
