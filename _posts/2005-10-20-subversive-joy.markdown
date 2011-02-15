--- 
layout: post
wordpress_id: 35
title: Subversive Joy
wordpress_url: http://www.alieniloquent.com/?p=35
--- |+
A year and a half ago, when I started working at my company, one of the first
things I saw was the version control they used: Visual Source Safe. My
immediate reaction was, "Why don't we ditch this crap and use a real version
control tool like Subversion, or at least CVS." Unsurprisingly, the response
was, "We'd love to ditch this crap, but we can't afford to right now, and we
don't know what we want to switch to anyway." So began our journey.

The first task set before us was to decide which version control we wanted to
switch to. There was [Vault][1], which would have theoretically been a drop-in
replacement. I like [Eric][2], but I don't like the VSS/Vault paradigm.
Another contender was [StarTeam][3], since we're a Delphi shop, but it is
expensive. There were a few other contenders as well, but they all had one
thing in common: they cost money. I sat in the corner as this list was
assembled and kept piping up, "What about Subversion? It's fast, it's fully-
featured, and it's free." It was added to the list, but not as a serious
contender for starters.

Then I bought [Pragmatic Version Control Using Subversion][4], and devoured
it. I hadn't even known how cool Subversion was, and now I _really_ wanted to
use it. The first thing I did upon completing the book was give it to the
senior engineer in charge of deciding what we were going to move to. He worked
his way through the book and agreed with me: it was cool. Finally, Subversion
won the Jolt award last year, so it was decided. Needless to say, I was
excited.

That was months ago. We were in the middle of a big development push and
couldn't afford the time to port everything over. So, it was decided when we
released, we'd switch. Well, that time came and went in late August, and we
are still on VSS. We have, however, finally been feeling severe pain about
merging and various other activities in version control. So, I finally got the
go ahead to start working on it. Today I set up our Subversion server and got
it ready for prime time usage (pending blog post about how). Tomorrow I will
actually move our source tree into Subversion. Next week, we'll cut over. I am
very excited.

   [1]: http://sourcegear.com/vault/

   [2]: http://www.notalegend.com

   [3]: http://www.borland.com/starteam/

   [4]: http://www.pragmaticprogrammer.com/titles/svn/index.html

