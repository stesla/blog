---
title: A little more lambda
---
[Alonzo Church][1] invented the lambda calculus. He also figured out how to
encode many kinds of data as lambda expressions. Take your simple booleans,
for example.

This is true:

~~~~ {.code}
fn x y. x
~~~~

And this is false:

~~~~ {.code}
fn x y. y
~~~~

That makes the identity function the if then else construct:

~~~~ {.code}
> (fn p. p) (fn x y. x) a b;
a
> (fn p. p) (fn x y. y) a b;
b
~~~~

And similarly you can get a logical and:

~~~~ {.code}
> (fn p. p) ((fn p q. p q p) (fn x y. x) (fn x y. x)) a b;
a
> (fn p. p) ((fn p q. p q p) (fn x y. x) (fn x y. y)) a b;
b
> (fn p. p) ((fn p q. p q p) (fn x y. y) (fn x y. x)) a b;
b
~~~~

Fiddling around with these church booleans revealed several bugs in my code,
which I've fixed. I've additionally added a new node to the parse tree to
represent the () grouping that is typed into the code so that when it is
formatted for display it looks better.

You can get the newest code [here][2].

   [1]: http://en.wikipedia.org/wiki/Alonzo_Church

   [2]: http://www.alieniloquent.com/code/lambda/

