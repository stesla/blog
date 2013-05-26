---
title: Samuel's Lambda and the Y Combinator
---
So I'm taking "Principles of Programming Languages" at UNO with [Dr.
Winter][1]. There is a group project, and he is letting me do the project on
my own. The project is to make a small imperative (and [Turing-complete][2])
language.

Well, as you may or may not know, I'm crazy about functional languages. I love
them. So, while I have to write an imperative language for the project, I
decided to spend some of the precious free time I have writing a functional
one instead. As of now, my language (Samuel's Lambda, or SL for short) is
Turing-complete.

My test for this was that I could calculate the factorial using the most
venerable of functional programming tools: [the fixed point combinator][3]
(a.k.a. the Y combinator).

For my example of recursion, I'll show you the factorial. What sort of
discussion of recursion would this be if I didn't?

{% highlight text %}
let
  val y = fn f =>
      (fn g => g g) (fn g => f (fn x => g g x))
  val fac = fn f =>
            fn n =>
               if eq n 0 then 1
               else multiply n (f (subtract n 1))
in y fac 5
end
{% endhighlight %}

When run at the SML/NJ prompt:

{% highlight text %}
- SLParser.evalPrintFile("examples/factorial.sl");
120
val it = () : unit
{% endhighlight %}

It helps that I'm working with a functional language to start with. That makes
implementing things such as closures and `let` nearly trivial. I'm going to
add static type checking (sans-inferencing like ML after which the syntax has
been modeled) and then I'll be done with SL. It has been a fun little
exercise.

   [1]: http://faculty.ist.unomaha.edu/winter/

   [2]: http://en.wikipedia.org/wiki/Turing_complete

   [3]: http://en.wikipedia.org/wiki/Fixed_point_combinator

