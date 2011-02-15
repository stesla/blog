--- 
layout: post
wordpress_id: 112
title: The Lambda Calculus
wordpress_url: http://blog.alieniloquent.com/2007/12/23/the-lambda-calculus/
--- |+
Earlier this fall I wrote a little functional programming language. However,
the guts of it were not based on the [lambda calculus][1]. I used more of a
denotational semantics approach to the evaluation, which worked fine. But, I
still wanted to implement an actual lambda calculus interpreter.

So, now that I am done with school and have some free time, I threw a little
something together. I used it as an introductory project to [OCaml][2], and
really enjoyed writing it.

So what is the lambda calculus, you might ask?

There are three basic concepts in lambda calculus. There are variables:

x

There are abstractions:

fn x. x

And there are applications:

f x

Applications are left associative so:

f x y

is the same as:

(f x) y

So for a more complicated example from the REPL:

{% highlight text %}

> (fn f. fn x. f x) (fn y. y) z;

z

{% endhighlight %}

The first part declares a function which binds `f` and returns a function
which binds `x` and returns the application of `f` to `x`. We pass to that the
identity function `(fn y. y)` and the variable `z`. That all reduces to just
`z`.

You can download my code [here][3]. I will be posting snippets of it in future
posts. I will also be blogging as I extend it to add more features.

   [1]: http://en.wikipedia.org/wiki/Lambda_calculus

   [2]: http://caml.inria.fr/

   [3]: http://www.alieniloquent.com/code/lambda/

