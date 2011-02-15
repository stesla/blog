---
layout: post
wordpress_id: 270
title: <code>you.would? like(:fries).with(that)</code>
wordpress_url: http://blog.alieniloquent.com/?p=270
---
I was recently linked to an insightful article about how [software is
hard][1]. He makes several good points, but the one that sticks out most in my
mind is the following:

> Every software engineer has a low opinion of the way we develop software.
Even the term "software engineering," Rosenberg writes, is a statement of
hope, not fact. He quotes the 1968 NATO [Software Engineering Conference][2]
that coined the term: "We undoubtedly produce software by backward
techniques." "We build systems like the Wright brothers built airplanes--build
the whole thing, push it off the cliff, let it crash, and start over again."
Certainly statements that could still be made forty years later.

That is so true, especially when it comes to the programming languages we use.
Most of the languages with widespread use in industry today are essentially
thirty years old, and even those are just the latest in a long [lineage][3]
that leads back to FORTRAN and Lisp in the late sixties. Languages in today's
generation don't do anything for you that their ancestors didn't do forty
years ago.

What's worse is that more modern languages exist--they have existed for
decades--yet they don't find mainstream adoption. For the last twenty years
the main thrust of innovation in the software industry has been focused on
people and process rather than languages and tools. Modern languages are
dismissed as academic, and industry experts are struggling to find new and
better ways to build useful systems with ancient technology.

We are rapidly approaching a singularity after which the descendants of
FORTRAN and Lisp will be grossly inadequate for writing programs. The current
rise of concurrent programming is just the beginning. As time plods on and
concurrency and distributed programming become more commonplace, we are going
to have to develop new ways of writing software.

This really isn't news. Software is hard. Concurrent software is harder. We
know that. The solutions offered today are primitive, and not much different
from the thinking over thirty years ago. Whether you use some shared-memory
model or some message-passing model, writing concurrent programs is a mentally
exhausting process with a lot of work centered around getting the parallelism
right.

Another problem we face is the increasing ubiquity of software. In the modern,
Western world it is nearly impossible to take ten steps without walking past
at least a hundred semiconductors. Many of those little bits of manufactured
silicon are running some sort of program (whether it is firm or software). We
passed the point in time where our lives are computerized a long time ago, and
many people didn't even notice. Your life relies on software in vehicles,
hospitals and banks.

With software becoming so commonplace, the burden to produce quality software
is greater than ever. Software verification is a critical area of the
development process that has been shit on and forgotten for over forty years.
Today's state of the art in quality assurance is a combination between
exploratory testing and developer-written regression suites. In other words,
if you don't think about verification, then none happens at all.

These problems are both huge. We must solve them, and wetware solutions will
be woefully inadequate. We need to adopt and develop new languages and tools
that directly address these issues. This is the area where we have _regressed_
from forty years ago. Back then programmers quickly grew impatient with
wetware solutions and would write software to automate things and introduce
abstractions: programming languages, parser and scanner generators, build
automation tools, build configuration tools, stream editors, and the list just
goes on.

Somewhere in the last forty years, we just gave up. As an industry, we decided
that languages and tool-chains were fundamentally solved problems, and all we
needed to do was evolve the syntax and libraries. We have eschewed the
[virtuous][4] path of software development in favor of a naively pragmatic
alternative. Rather than working smarter, we try to work faster. Rather than
finding smarter people, we just find _more_ people. These days, there's an
alternative to working in food service: [programming][5].

   [1]: http://www.gamearchitect.net/Articles/SoftwareIsHard.html

   [2]: http://homepages.cs.ncl.ac.uk/brian.randell/NATO/NATOReports/index.html

   [3]: http://www.levenez.com/lang/

   [4]: http://c2.com/cgi/wiki?LazinessImpatienceHubris

   [5]: http://steve.yegge.googlepages.com/choosing-languages

