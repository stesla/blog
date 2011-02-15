---
layout: post
wordpress_id: 175
title: Cargo Cult on Rails
wordpress_url: http://blog.alieniloquent.com/?p=175
---
I've been coding in Ruby for a long time, and I love the language. It has all
the object-oriented power of Smalltalk, the functional power of Lisp, and the
concision of Perl. It is so powerful that it enables programmers to devise
entirely new ways of writing software, and that is the hallmark of a good
language.

The trouble is that some of these new ways of programming aren't good ways of
programming. Over the last five years we've seen the ascent of Ruby on Rails
and the Rails Way of doing things. I'll be the first to admit that Rails
revolutionized web development. But it has taught an entire generation of web
developers to think like a cargo cult, and that's not good.

## Convention Over Thinking

Rails' mantra of convention over configuration is brilliant. I love it. But
somewhere along the line the message got skewed and people started thinking
that everything should be a convention. Every web app should be written
exactly the same, and the only thing that changes is the bits between the
browser and the RDBMS. Rails is the glue that holds it together, and you just
need to describe the data.

The trouble is, that kind of thinking is just plain wrong. As the folks at
Twitter discovered, not every problem is a nail that can be hammered.
Sometimes they're screws, or bolts, and they need different tools to fasten
them down. That's why writing software is hard. It requires thinking. If
you're not thinking to see if the conventions are actually correct for your
application, then you're doing it wrong.

## Rockstars and Groupies

The Rails community seems to love its rockstar programmers. Big personalities
like DHH, Ezra, and Yehuda are practically worshipped. When they speak their
minds its taken by many as gospel truth. Because conventions are so important,
they need somebody to tell them what the conventions are, so the rails
groupies flock to their rockstars to be told how to write their applications.

People, that's just sick. You need to be thinking for yourselves. Who cares
what DHH thinks? What do _you_ think? Your application should not look like
some celebrity programmer tells you it should. Your application should be
custom-built to solve the problems in your domain. You are the expert in your
domain, so look no further than yourself.

## Framework-Driven Development

What it comes down to is that Rails developers are just that: Rails
developers. They're not software developers, at least not most of them. Their
entire software devlopment world is built up around this framework. They only
know how to do things the Rails Way. Their framework dictates how their
systems are designed instead of the problems the systems are designed to
solve.

The real irony here is that this is exactly the kind of problem Rails was
running away from. Five years ago, you couldn't write a Java-based web-app if
you were just some developer who happened to know Java. You had to be a Struts
developer or a J2EE developer because you needed to know the deep inner
workings of the framework you were going to use. Now Rails is the same way.

Congratulations, Rails, you've turned into your parents.

