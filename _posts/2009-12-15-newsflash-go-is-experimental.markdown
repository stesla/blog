--- 
layout: post
wordpress_id: 250
title: "Newsflash: Go is Experimental"
excerpt: "Go is <em>incomplete</em> and <em>experimental</em>. Even recently it went through a fundamental syntax change (making semi-colons optional virtually everywhere). The Go authors are very willing to listen to suggestions that are backed up with code. So, what I have to say to the author is: nut up or shut up."
wordpress_url: http://blog.alieniloquent.com/?p=250
--- |+
Saw a [rant][1] today about [Go][2]. I was genuinely impressed with the amount
of piss and vinegar the author managed to conjure up over this. He's more
upset about this language than I am excited about it, and I wrote a
[library][3] in it and an [article][4] about it already. He brought up some
valid points, but overall, he completely missed the mark.

The author brings up two major gripes with Go: it has an ugly syntax, and it
lacks innovation. I'll cede both points, but with a few reservations. Go is
experimental. It's not complete yet. In fact, the language is still changing.
The Go authors released it so that other people could see what they were
working on, but even they don't suggest using it for anything critical. Go is
not intended to replace Java, or even C++ (though maybe). It's an upgrade to
C.

### Ugly Syntax!

I'm not a huge fan of Go's syntax. It's a curly-brace language. I do, however,
understand why the syntax looks like it does. It is a similar reason to why
JavaScript looks like Java. I don't have a direct line into the Go authors'
thoughts, but I'd wager they wanted something familiar. So we end up with
another curly-brace language. I forgive them.

That said, I'd love to hear what the author suggests as alternative syntaxes
for the statements he calls out. In fact, I'm sure the Go authors would love
to hear any constructive suggestions he might have on a better syntax for
these things. I know that the [mailing list][5] has frequent discussion
regarding ways to improve the syntax. The Go authors are willing to consider
changes, especially changes that are proposed along with patches that show how
to implement them.

I do want to call him out on his hate of the condition initializer. That new
syntax, along with multiple return values allows programmers to replace C code
like this:

{% highlight text %}int result = someCall();

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

}{% endhighlight %}

With Go code like this:

{% highlight text %}if result, err := someCall(); if err != nil {

/* Handle Error */

} else {

/* Do something with result */

}{% endhighlight %}

Notably, in the Go code, both `result` and `err` are scoped to just inside the
if statement. They don't clutter the surrounding block. I'll talk more about
this pattern in the part about exceptions.

### Innovation?

I will take the author's statement that Go lacks innovation, and go one step
further. Go offers _no real innovation_ over other languages that exist today.
The three things that the author calls out aren't really innovative. Analogs
to both channels and goroutines have been present in Erlang for twenty years.
The interface-only inheritance is just a way to apply the duck-typing concepts
of Ruby and Python to a statically type-checked language. There is nothing new
to see here. Move along.

The point the author fails to see is that Go is not meant to innovate
programming theory. It's meant to innovate programming practice. This is an
upgrade to C. It's a language that applies the innovations of the last thirty
years to the systems world, where the state of the art is still _portable
assembler_. So no, the language isn't introducing brilliant new ideas. It's
taking tested old ideas, and introducing them into a new arena.

### Exceptions

_Oh no!_ Go doesn't have exceptions. Everybody knows that every _modern_
language has exceptions. You can't write _real_ programs without non-local
transfer of control. Wait? What's that you say? C doesn't have exceptions?
[People][6] [still][7] [code][8] [in][9] [that][10] [unusable][11]
[language][12]?

Don't get the wrong idea. I think exceptions are useful. I use them in
languages that have them. I also strongly dislike the C idiom of returning
out-of-band values to indicate errors (thus requiring that some value _be_ out
of band). However, I think Go has a reasonable way to handle this. Using
multiple return values, named return variables, and the comma-error pattern, a
decent error reporting facility can be created without exceptions.

Here is an example:

{% highlight text %}func magic() (result int, err io.Error) {

if err = moreMagic(); err != nil {

/* Calculate result */

}

return;

}{% endhighlight %}

This allows the error from `moreMagic` to propagate up without reserving an
out-of-band result value. It provides no more syntactic overhead than explicit
exceptions in Java (and arguably less).

The argument was made that people can fail to check return codes. People can
be just as stupid in languages with exceptions. I've lost count how many times
I've seen this in Java:

{% highlight text %}try {

/* Do something that might blow up */

} catch {

}{% endhighlight %}

The catch block is empty on purpose. People do that. They catch _all_
exceptions and then _do nothing_ with them _on purpose_. You can be an idiot
in any language.

### Generics

I'll be honest. I've hated every implementation of generics I've seen in non-
functional languages. That is to say, C++, Java, and C#. They are a poor
attempt at implementing the polymorphic types that ML-derivatives have enjoyed
for decades. The syntax for them is inevitably terrible and the semantics just
as awful. However, plenty of modern languages don't have generics.

The author is whining because the language doesn't have his favorite language
feature.

### New Jersey style

The author wants to suggest that the Go authors subscribe to the [New Jersey
school][13] of programming. He suggests that his two favorite features
(exceptions and generics) aren't in the language because the Go authors were
too scared to implement them. He even cites the [Language Design FAQ][14] as
proof.

If this were a reading report, I would give it a grade of D, maybe a C- if I
felt charitable. Why? Because the author of the article clearly had nearly no
comprehension of the FAQ. He suggests that the Go authors decided not to
include the features because they couldn't figure out how to implement them.
When the truth is, _Go is incomplete_ and they cannot figure out how to
include exceptions or generics _yet_ and are _open to suggestions_.

Just like with his syntax improvements, I'd encourage the author to post to
the mailing lists with suggestions on how to implement exceptions and generics
in such a fashion that they fit in with the philosophy of the language. The Go
community is interested in those topics. The discussions are happening right
now.

### TL;DR

Go is _incomplete_ and _experimental_. Even recently it went through a
fundamental syntax change (making semi-colons optional virtually everywhere).
The Go authors are very willing to listen to suggestions that are backed up
with code. So, what I have to say to the author is: nut up or shut up.

   [1]: http://monoc.mo.funpic.de/go-rant/

   [2]: http://golang.org

   [3]: http://github.com/stesla/gospecify

   [4]: http://www.engineyard.com/blog/2009/ready-set-go/

   [5]: http://groups.google.com/group/golang-nuts

   [6]: http://www.linux.org/

   [7]: http://httpd.apache.org/

   [8]: http://git-scm.com/

   [9]: http://code.google.com/p/redis/

   [10]: http://github.com/erlang/otp

   [11]: http://www.gtk.org/

   [12]: http://memcached.org/

   [13]: http://www.jwz.org/doc/worse-is-better.html

   [14]: http://golang.org/doc/go_lang_faq.html

