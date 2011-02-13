--- 
wordpress_id: 113
layout: post
title: Currying Function Parameters
wordpress_url: http://blog.alieniloquent.com/2007/12/23/currying-function-parameters/
---
One of the first things I wanted to do to improve the readability of my language was to add the currying of function parameters. Since it is such a common pattern to have three or four abstractions right in a row to bind variables, there is a syntax for expressing them more consisely.

So this:
<p class="code">fn x. fn y. fn z. x y z</p>

Becomes this:
<p class="code">fn x y z. x y z</p>

Adding the code do this was nearly trivial, and all in the parser. First I wrote a function that given a list of variables and an expression for the body, would be able to construct the parse tree for a curried function:

<pre class="code">
let curry ids body =
  List.fold_right (fun id expr -&gt; Abstraction(id, expr)) ids body
</pre>

Then I took the existing production for recognizing expressions:

<pre class="code">
expr:
  aexprs {apply $1}
| FN VAR PERIOD expr {Abstraction ($2, $4)}
;
</pre>

And turned it into this:
<pre class="code">
expr:
  aexprs {apply $1}
| FN ids PERIOD expr {curry $2 $4}
;

ids:
  VAR {[$1]}
| VAR ids {$1::$2}
;
</pre>

That <code>ids</code> production is using the OCaml <code>::</code> operator which performs the cons operation. So as I recurse on the right, I'm building up a list and consing each new id onto it all the way up.

And just like that I've added currying to my language.
