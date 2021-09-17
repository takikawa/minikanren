#lang scribble/manual

@(require (for-label minikanren racket/base))

@title{MiniKanren: logic programming in Scheme}

@defmodule[minikanren]

An embedding of logic programming in Scheme.

The miniKanren language in this package is the language presented in
Byrd and Friedman's "From variadic functions to variadic relations"
[1]; it is a descendant of the language presented in Friedman, Byrd,
and Kiselyov's @emph{The Reasoned Schemer} [2]. The code itself was written
by (in alphabetical order) Will Byrd, Dan Friedman, Oleg Kiselyov, and
Chung-Chieh Shan.

Minikanren provides the following new core forms, which comprise
the core of the miniKanren language. These forms are all described in
much greater detail in Byrd and Friedman's "From variadic functions to
variadic relations" [1], which those interested in learning how to
use this library should consult. The documentation here is included
only for reference.

@defform[(== e_1 e_2)]{
Introduces a new goal that unifies two values.
}

@defform[(conde ((goal_1 goal_1a ...) (goal_2 goal_2a ...) ...))]{
Introduces a new goal that behaves analagously to @racket[cond].
}

@defform[(condi ((goal_1 goal_1a ...) (goal_2 goal_2a ...) ...))]{
@racket[condi] behaves like @racket[conde], except that its values are interleaved.
}

@defform[(fresh (x ...) goal_0 goal_1 ...)]{
Introduces a new logic variable bound to each @racket[_x] in the scope of its
body, each clause of which should be a goal.
}

@defform[(run n (x) goal_0 goal_1 ...)]{
Finds up to @racket[_n] ways to instantiate the logic variable bound to
@racket[_x] such that all the goals in its body succeed. (If @racket[_n] is
@racket[#f], then run finds all satisfying instantiations for @racket[_x].)
}

@defform[succeed]{
It is a goal that succeeds.
}

@defform[fail]{
It is a goal that fails; it is unsuccessful.
}

Minikanren also provides the following helpers:

@defform[(run* (x) goal_0 goal_1 ...)]{
Equivalent to @racket[(run #f (x) goal_0 goal_1 ...)].
}

@defform[(conda ((goal_1 goal_1a ...) (goal_2 goal_2a ...) ...))]
@defform[(condu ((goal_1 goal_1a ...) (goal_2 goal_2a ...) ...))]{
Variants of @racket[conde] that correspond to the committed-choice of Mercury
and are used in placb of Prolog's cut.
}

@defform[(project (x ...) goal_1 goal_2 ...)]{
Applies the implicit substitution to zero or more lexical variables, rebinds
those variables to the values returned, and evaluates the goal expressions in
its body. The body of a @racket[project] typically includes at least one begin
expression --- any expression is a goal expression if its value is a miniKanren
goal. @racket[project] has many uses, such as displaying the values associated
with variables when tracing a program.
}

References

[1] Byrd, William and Friedman, Daniel. "From variadic functions to
variadic relations: a miniKanren perspective." In Proceedings of the
2006 Scheme and Functional Programming Workshop, 2006. Available
online: http://scheme2006.cs.uchicago.edu/12-byrd.pdf.

[2] Friedman, Daniel, Byrd, William, and Kiselyov, Oleg. The Reasoned
Schemer. MIT Press, 2005.
