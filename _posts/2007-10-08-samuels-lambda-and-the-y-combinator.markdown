--- 
wordpress_id: 108
layout: post
title: Samuel's Lambda and the Y Combinator
wordpress_url: http://blog.alieniloquent.com/2007/10/08/samuels-lambda-and-the-y-combinator/
---
So I'm taking "Principles of Programming Languages" at UNO with <a href="http://faculty.ist.unomaha.edu/winter/">Dr. Winter</a>.  There is a group project, and he is letting me do the project on my own.  The project is to make a small imperative (and <a href="http://en.wikipedia.org/wiki/Turing_complete">Turing-complete</a>) language.

Well, as you may or may not know, I'm crazy about functional languages.  I love them.  So, while I have to write an imperative language for the project, I decided to spend some of the precious free time I have writing a functional one instead.  As of now, my language (Samuel's Lambda, or SL for short) is Turing-complete.

My test for this was that I could calculate the factorial using the most venerable of functional programming tools: <a href="http://en.wikipedia.org/wiki/Fixed_point_combinator">the fixed point combinator</a> (a.k.a. the Y combinator).

For my example of recursion, I'll show you the factorial.  What sort of discussion of recursion would this be if I didn't?

<pre class="code">let
  val y = fn f =>
      (fn g => g g) (fn g => f (fn x => g g x))
  val fac = fn f =>
            fn n =>
               if eq n 0 then 1
               else multiply n (f (subtract n 1))
in y fac 5
end
</pre>

When run at the SML/NJ prompt:

<pre class="code">- SLParser.evalPrintFile("examples/factorial.sl");
120
val it = () : unit</pre>

It helps that I'm working with a functional language to start with.  That makes implementing things such as closures and <code>let</code> nearly trivial.  I'm going to add static type checking (sans-inferencing like ML after which the syntax has been modeled) and then I'll be done with SL.  It has been a fun little exercise.
