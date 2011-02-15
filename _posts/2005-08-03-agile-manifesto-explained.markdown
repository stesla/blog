--- 
layout: post
wordpress_id: 17
title: "Agile Manifesto: Explained"
wordpress_url: http://www.alieniloquent.com/?p=17
--- |+
A link to an article titled, ["Agile Programming: How to doom your software
project"][1] by [Steve Friedl][2] was pasted into my IM window this evening.
Steve wrote an excellent piece about [being a consultant][3] back in January,
and I enjoyed it quite thoroughly.

Seeing as I work in an agile shop, I was drawn to the title of this article
like a moth to a flame. It seems that Steve found the [agile manifesto][4],
and misunderstood each line item. Now, it's really not his fault. The
manifesto could use a companion document that explains it, or perhaps it
should be rewritten such that it elaborates. But, the end result was that
Steve came away thinking that all of us agile guys have had a bunch of kool-
aid and are about to plunge our companies into deep pits of failure with these
kooky ideas. This just isn't true (well, except the kool-aid, but I mean, that
talking pitcher is just so charismatic, I have to drink from its head). So I
am here to explain the four points and hopefully allay any misconceptions you
might have about them.

**"\[We value\] _individuals and interactions_ over processes and tools"**


Steve seems to interpret this to mean that agile practitioners would rather
throw people at a problem than find a good tool for it. That is simply not
true. Even cursory research into agile practices (including surfing the [Agile
Alliance][5], which Steve linked) would reveal a heavy preference toward tool
usage. In fact, agile practitioners strive to automate as much as they can so
that they can focus on the hard problems.

The idea behind this principle is that our teams should place a higher value
on our people than on which tools or processes that we use. The people that
comprise a team are the most expensive, and most valuable resources that the
business will put into producing software, and so it is important that they
are valued. Individuals should be given the power to get things done the best
way that they can. They should be given a license to be creative and innovate.

The people on a software team are the most important part. If the tools are
getting in the way, then they need to be replaced. If the process is slowing
things down, then it needs to be revised. Tools and processes are a means for
the people on the team to get to and end. Each engineer should be able to get
to those ends by the best, most efficient way they can.

Steve also says that in his experience, "a lot more projects have been sunk by
not-enough-tools than by not-enough-interaction." I'd wager that when he
thinks of interaction, he thinks of meetings that last hours--nearly useless
meetings that mostly serve to bore all those in attendance. That is, indeed,
not going to help a project succeed. Thankfully, that's not what agile
practioners mean.

Agile practioners realize that developing software is not a technical
business, at its core. It is a social business. It is about providing people a
way to do something that they couldn't do before. It is about making things
easier for people to do. It is about enabling people to interact with
technology.

At its core, software is all about individuals and interactions. Whether its a
user interacting with his computer, a salesperson interacting with her client,
a programmer interacting with his customer, or a programmer interacting with
the code, software is all about individuals and interactions. Agile
practitioners realize this and strive to make the interactions efficient and
enjoyable so that the individuals can do more and enjoy themselves.

**"\[We value\] _working software_ over comprehensive documentation"**


This is an easy one to misunderstand, and maybe Steve didn't. There are two
schools of thought when it comes to specifications: they are either redundant
information that is duplicated in the code, or they are essential tools to
communicating what the software is supposed to do. Whether or not Steve
understood the manifesto, he clearly adheres to the latter school.

Agile practitioners realized that they were writing documentation that
described how the program was supposed to behave and then they were writing
code that described how the program was supposed to behave. Yes, you read that
correctly. That is all code is: a description of how the program is supposed
to behave. It's expressed in very precise terms with very specific grammar,
but it is a description of the program's behaviour nonetheless.

What's more, because we're writing everything down twice (or more, depending
on your process), there are two (or more) places that have to change if those
behaviours have to change. And, unless you've got some magic voo-doo that I
don't know about, there's nothing that _forces_ you to keep your documentation
in sync (even the best literate code tools don't care if your documentation
comments are out of sync with the real behavior).

The solution is to make your specification out of code. Write tests using
technical-facing tools like [these][6] and write tests using business-facing
tools like [this][7]. Use these tests to specify exactly what the behavior is:
if the tests pass, the program works. What's more, these tests actually _use_
the code and _inspect_ its behavior. So, if its behavior changes, the tests
will know automatically. Furthermore, the tests serve as examples of how to
use the code. With tests like these, you don't really need a specification
document anymore.

We all know that we should strive to remove duplication from our code. That's
why we refactor and that's why we design. Agile practitioners strive to remove
duplication from our processes as well. The less we duplicate information, the
more efficiently we can change and the more quality those changes will have.
This is what we mean when we say we value working software over comprehensive
documentation.

**"\[We value\] _customer collaboration_ over contract negotiation."**


Steve's right. Customers never know what they want. What he's wrong about is
that negotiating a contract up-front is the way to find out what they want.
The truth is that the only way for a customer to find out what they want, and
more importantly what they _need_, is to _use the software_. This is why agile
teams place such a high priority on delivering working software quickly.

What's more, agile teams tend to deliver their software in small increments
and put in the features that are the absolutely most important first. I bet
that if you asked a customer to come up with the list of features that would
have to be in the product to make it have _any_ business value at all, they
wouldn't have a hard time ("a text editor must be able to display text files,
change text, and save text files"). Once you have this super-simple, barely
valuable product _in the customer's hands_ they can give you good feedback as
to what should go in next. Working this way, the customer gets exactly what
they need and they get it as quickly as they can. It also allows their
understanding of what they need to grow _while they have a working program_.

Give me a customer who wouldn't want to have a working program that delivers
business value after two or three weeks, and I'll eat the paper you printed
this out on (assuming, of course, you printed this out).

**"\[We value\] _responding to change_ over following a plan"**


Steve returns once again to software specifications. We know what agile
practitioners think of duplicating data, but that is all about documenting
what the software does. How are we supposed to decide how to get from now to a
time when it does these things? We plan. But built into that plan is the
_expectation of change_.

Agile methods build in the assumption that change will happen and it will
happen often. So Steve's sentiment that "hopefully one has built a \[software\]
system that tolerates change," is reduced to "one built a software system that
tolerates change." Assume that there will be technical changes and ensure that
your software is easy to change by keeping it well factored and easy to
understand. Assume that there will be business changes so deliver software in
short cycles.

Agile software development is all about embracing change and expecting it. As
the old adage goes: the only constant thing in software is change. Learn it,
love it, live it.


I'm certainly not the first person to say these things, and I probably won't
be the last. I hope reading this has shed a little light on what that
manifesto really means, but the best way is to get out there and _do it_. Try
an agile method on a project and see how you like it. If it works for you,
great. If it doesn't work for you, great, now you know. Sitting around and
speculating gets you nowhere.

   [1]: http://www.unixwiz.net/archives/2005/07/agile_programmi_1.html

   [2]: http://www.unixwiz.net/blog/

   [3]: http://www.unixwiz.net/techtips/be-consultant.html

   [4]: http://www.agilemanifesto.org/

   [5]: http://www.agilealliance.com/home

   [6]: http://c2.com/cgi/wiki?TestingFramework

   [7]: http://fit.c2.com/

