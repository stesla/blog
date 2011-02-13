--- 
wordpress_id: 112
layout: post
title: The Lambda Calculus
wordpress_url: http://blog.alieniloquent.com/2007/12/23/the-lambda-calculus/
---
Earlier this fall I wrote a little functional programming language. However, the guts of it were not based on the <a href="http://en.wikipedia.org/wiki/Lambda_calculus">lambda calculus</a>. I used more of a denotational semantics approach to the evaluation, which worked fine. But, I still wanted to implement an actual lambda calculus interpreter.

So, now that I am done with school and have some free time, I threw a little something together. I used it as an introductory project to <a href="http://caml.inria.fr/">OCaml</a>, and really enjoyed writing it.

So what is the lambda calculus, you might ask? 

There are three basic concepts in lambda calculus. There are variables:

<p class="code">x</p>

There are abstractions:

<p class="code">fn x. x</p>

And there are applications:

<p class="code">f x</p>

Applications are left associative so:

<p class="code">f x y</p>

is the same as:

<p class="code">(f x) y</p>

So for a more complicated example from the REPL:

{% highlight text %}
> (fn f. fn x. f x) (fn y. y) z;
z
{% endhighlight %}

The first part declares a function which binds <code>f</code> and returns a function which binds <code>x</code> and returns the application of <code>f</code> to <code>x</code>. We pass to that the identity function <code>(fn y. y)</code> and the variable <code>z</code>. That all reduces to just <code>z</code>.

You can download my code <a href="http://www.alieniloquent.com/code/lambda/">here</a>. I will be posting snippets of it in future posts. I will also be blogging as I extend it to add more features.
