--- 
wordpress_id: 18
layout: post
title: The many values of True
wordpress_url: http://www.alieniloquent.com/2005/08/04/the-many-values-of-true/
---
So Brian and I are refactoring some code.  One of the things we want to do is get rid of a property that exposes an implemenation-specific detail and replace it with a boolean method that made the single check that those details were used for.  Naturally, this is in code well-covered with tests (<em>well</em> covered, thank goodness), and they let us know right away when we made a stupid mistake.

Before our refactor:

<pre class="code">Assert.Equals(1, object.Detail, &quot;Details don't match&quot;)</pre>

After our refactor: 

<pre class="code">Assert.Equals(object.hasDetail(1), &quot;Details don't match&quot;)</pre>

That is a perfectly valid test...syntactically.  However it results in the following error message:

<pre class="code">&quot;True&quot; does not equal &quot;Details don't match&quot;</pre>

Now, those error messages are there for a reason: for us to ignore them.  Brian and I spent at least five minutes scratching our head and stepping through the test before we noticed the Equals.  Sure enough, had we bothered to read the message, it would've been only 30 seconds to our green bar.
