---
title: Currying Function Parameters
---
One of the first things I wanted to do to improve the readability of my
language was to add the currying of function parameters. Since it is such a
common pattern to have three or four abstractions right in a row to bind
variables, there is a syntax for expressing them more consisely.

So this:

~~~~ {.code}
fn x. fn y. fn z. x y z
~~~~

Becomes this:

~~~~ {.code}
fn x y z. x y z
~~~~

Adding the code do this was nearly trivial, and all in the parser. First I
wrote a function that given a list of variables and an expression for the
body, would be able to construct the parse tree for a curried function:

~~~~ {.code}
let curry ids body =
  List.fold_right (fun id expr -> Abstraction(id, expr)) ids body
~~~~

Then I took the existing production for recognizing expressions:

~~~~ {.code}
expr:
  aexprs {apply $1}
| FN VAR PERIOD expr {Abstraction ($2, $4)}
;
~~~~

And turned it into this:

~~~~ {.code}
expr:
  aexprs {apply $1}
| FN ids PERIOD expr {curry $2 $4}
;

ids:
  VAR {[$1]}
| VAR ids {$1::$2}
;
~~~~

That `ids` production is using the OCaml `::` operator which performs the cons
operation. So as I recurse on the right, I'm building up a list and consing
each new id onto it all the way up.

And just like that I've added currying to my language.
