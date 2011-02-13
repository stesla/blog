--- 
wordpress_id: 54
layout: post
title: "Inexplicable Key-Chord #572"
wordpress_url: http://www.alieniloquent.com/?p=54
---
There's a plugin for the Delphi IDE called GExperts.  This plugin provides a metric ton of addons.  The one that we use it for here at my office is Grep.  In most ways it is superior to Delphi's find-in-files functionality.  There are two key-chords associated with grep: Alt-Shift-S to bring up the search screen and Alt-Ctrl-R to bring up the search results.

That's great.  The problem is, I do one way more than the other.  So I remember the correct <a href="http://www.catb.org/jargon/html/B/bucky-bits.html">bucky bits</a> for grepping, but I frequently forget the correct ones for bringing up the search results.

Naturally, I will try Alt-Shift-R before Alt-Ctrl-R, figuring the bucky bits would be the same.  And for two years, nothing has ever happened.  Then one day last week, I'm sitting on an assignment statement like this:

{% highlight text %}Foo := Bar;{% endhighlight %}

I hit Alt-Shift-R and all of the sudden I've got this:

{% highlight text %}Bar := Foo;{% endhighlight %}

"Whisky tango foxtrot," I say, and hit again.  It flips again.  I try it on this:

{% highlight text %}Result := Foo(Bar);{% endhighlight %}

And I get:

{% highlight text %}Foo(Bar) := Result;{% endhighlight %}

Thanks, GExperts.

Now, I couldn't for the life of me figure out <em>why</em> anybody would want this feature.  Until I read the documentation.  It will also flip the direction of a for loop.  That is actually neat and cool.
