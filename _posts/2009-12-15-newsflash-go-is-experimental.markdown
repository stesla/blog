--- 
wordpress_id: 250
layout: post
title: "Newsflash: Go is Experimental"
excerpt: "Go is <em>incomplete</em> and <em>experimental</em>. Even recently it went through a fundamental syntax change (making semi-colons optional virtually everywhere). The Go authors are very willing to listen to suggestions that are backed up with code. So, what I have to say to the author is: nut up or shut up."
wordpress_url: http://blog.alieniloquent.com/?p=250
---
Saw a <a href="http://monoc.mo.funpic.de/go-rant/">rant</a> today about <a href="http://golang.org">Go</a>. I was genuinely impressed with the amount of piss and vinegar the author managed to conjure up over this. He's more upset about this language than I am excited about it, and I wrote a <a href="http://github.com/stesla/gospecify">library</a> in it and an <a href="http://www.engineyard.com/blog/2009/ready-set-go/">article</a> about it already. He brought up some valid points, but overall, he completely missed the mark.

The author brings up two major gripes with Go: it has an ugly syntax, and it lacks innovation. I'll cede both points, but with a few reservations. Go is experimental. It's not complete yet. In fact, the language is still changing. The Go authors released it so that other people could see what they were working on, but even they don't suggest using it for anything critical. Go is not intended to replace Java, or even C++ (though maybe). It's an upgrade to C.

<h3>Ugly Syntax!</h3>

I'm not a huge fan of Go's syntax. It's a curly-brace language. I do, however, understand why the syntax looks like it does. It is a similar reason to why JavaScript looks like Java. I don't have a direct line into the Go authors' thoughts, but I'd wager they wanted something familiar. So we end up with another curly-brace language. I forgive them.

That said, I'd love to hear what the author suggests as alternative syntaxes for the statements he calls out. In fact, I'm sure the Go authors would love to hear any constructive suggestions he might have on a better syntax for these things. I know that the <a href="http://groups.google.com/group/golang-nuts">mailing list</a> has frequent discussion regarding ways to improve the syntax. The Go authors are willing to consider changes, especially changes that are proposed along with patches that show how to implement them.

I do want to call him out on his hate of the condition initializer. That new syntax, along with multiple return values allows programmers to replace C code like this:

<pre class="code">int result = someCall();
if result < 0 {
  /* Handle Error */
} else {
  /* Do something with result */
}

/* Or worse... */

if (result = someCall()) < 0 {
  /* Handle Error */
} else {
  /* Do something with result */
}</pre>

With Go code like this:

</pre><pre class="code">if result, err := someCall(); if err != nil {
  /* Handle Error */
} else {
  /* Do something with result */
}</pre>

Notably, in the Go code, both <code>result</code> and <code>err</code> are scoped to just inside the if statement. They don't clutter the surrounding block. I'll talk more about this pattern in the part about exceptions.

<h3>Innovation?</h3>

I will take the author's statement that Go lacks innovation, and go one step further. Go offers <em>no real innovation</em> over other languages that exist today. The three things that the author calls out aren't really innovative. Analogs to both channels and goroutines have been present in Erlang for twenty years. The interface-only inheritance is just a way to apply the duck-typing concepts of Ruby and Python to a statically type-checked language. There is nothing new to see here. Move along.

The point the author fails to see is that Go is not meant to innovate programming theory. It's meant to innovate programming practice. This is an upgrade to C. It's a language that applies the innovations of the last thirty years to the systems world, where the state of the art is still <em>portable assembler</em>. So no, the language isn't introducing brilliant new ideas. It's taking tested old ideas, and introducing them into a new arena.

<h3>Exceptions</h3>

<em>Oh no!</em> Go doesn't have exceptions. Everybody knows that every <em>modern</em> language has exceptions. You can't write <em>real</em> programs without non-local transfer of control. Wait? What's that you say? C doesn't have exceptions? <a href="http://www.linux.org/">People</a> <a href="http://httpd.apache.org/">still</a> <a href="http://git-scm.com/">code</a> <a href="http://code.google.com/p/redis/">in</a> <a href="http://github.com/erlang/otp">that</a> <a href="http://www.gtk.org/">unusable</a> <a href="http://memcached.org/">language</a>?

Don't get the wrong idea. I think exceptions are useful. I use them in languages that have them. I also strongly dislike the C idiom of returning out-of-band values to indicate errors (thus requiring that some value <em>be</em> out of band). However, I think Go has a reasonable way to handle this. Using multiple return values, named return variables, and the comma-error pattern, a decent error reporting facility can be created without exceptions.

Here is an example:

<pre class="code">func magic() (result int, err io.Error) {
  if err = moreMagic(); err != nil {
    /* Calculate result */
  }
  return;
}</pre>

This allows the error from <code>moreMagic</code> to propagate up without reserving an out-of-band result value. It provides no more syntactic overhead than explicit exceptions in Java (and arguably less).

The argument was made that people can fail to check return codes. People can be just as stupid in languages with exceptions. I've lost count how many times I've seen this in Java:

<pre class="code">try {
  /* Do something that might blow up */
} catch {
}</pre>

The catch block is empty on purpose. People do that. They catch <em>all</em> exceptions and then <em>do nothing</em> with them <em>on purpose</em>. You can be an idiot in any language.

<h3>Generics</h3>

I'll be honest. I've hated every implementation of generics I've seen in non-functional languages. That is to say, C++, Java, and C#. They are a poor attempt at implementing the polymorphic types that ML-derivatives have enjoyed for decades. The syntax for them is inevitably terrible and the semantics just as awful. However, plenty of modern languages don't have generics.

The author is whining because the language doesn't have his favorite language feature.

<h3>New Jersey style</h3>

The author wants to suggest that the Go authors subscribe to the <a href="http://www.jwz.org/doc/worse-is-better.html">New Jersey school</a> of programming. He suggests that his two favorite features (exceptions and generics) aren't in the language because the Go authors were too scared to implement them. He even cites the <a href="http://golang.org/doc/go_lang_faq.html">Language Design FAQ</a> as proof.

If this were a reading report, I would give it a grade of D, maybe a C- if I felt charitable. Why? Because the author of the article clearly had nearly no comprehension of the FAQ. He suggests that the Go authors decided not to include the features because they couldn't figure out how to implement them. When the truth is, <em>Go is incomplete</em> and they cannot figure out how to include exceptions or generics <em>yet</em> and are <em>open to suggestions</em>.

Just like with his syntax improvements, I'd encourage the author to post to the mailing lists with suggestions on how to implement exceptions and generics in such a fashion that they fit in with the philosophy of the language. The Go community is interested in those topics. The discussions are happening right now.

<h3>TL;DR</h3>

Go is <em>incomplete</em> and <em>experimental</em>. Even recently it went through a fundamental syntax change (making semi-colons optional virtually everywhere). The Go authors are very willing to listen to suggestions that are backed up with code. So, what I have to say to the author is: nut up or shut up.
