--- 
wordpress_id: 114
layout: post
title: A little more lambda
wordpress_url: http://blog.alieniloquent.com/2007/12/23/a-little-more-lambda/
---
<a href="http://en.wikipedia.org/wiki/Alonzo_Church">Alonzo Church</a> invented the lambda calculus. He also figured out how to encode many kinds of data as lambda expressions.  Take your simple booleans, for example.

This is true:
<p class="code">fn x y. x</p>

And this is false:
<p class="code">fn x y. y</p>

That makes the identity function the if then else construct:
<pre class="code">
> (fn p. p) (fn x y. x) a b;
a
> (fn p. p) (fn x y. y) a b;
b
</pre>

And similarly you can get a logical and:
<pre class="code">
> (fn p. p) ((fn p q. p q p) (fn x y. x) (fn x y. x)) a b;
a
> (fn p. p) ((fn p q. p q p) (fn x y. x) (fn x y. y)) a b;
b
> (fn p. p) ((fn p q. p q p) (fn x y. y) (fn x y. x)) a b;
b
</pre>

Fiddling around with these church booleans revealed several bugs in my code, which I've fixed.  I've additionally added a new node to the parse tree to represent the () grouping that is typed into the code so that when it is formatted for display it looks better.

You can get the newest code <a href="http://www.alieniloquent.com/code/lambda/">here</a>.
