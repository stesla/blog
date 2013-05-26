---
title: The many values of True
---
So Brian and I are refactoring some code. One of the things we want to do is
get rid of a property that exposes an implemenation-specific detail and
replace it with a boolean method that made the single check that those details
were used for. Naturally, this is in code well-covered with tests (_well_
covered, thank goodness), and they let us know right away when we made a
stupid mistake.

Before our refactor:

{% highlight c# %}
Assert.Equals(1, object.Detail, "Details don't match")
{% endhighlight %}

After our refactor:

{% highlight c# %}
Assert.Equals(object.hasDetail(1), "Details don't match")
{% endhighlight %}

That is a perfectly valid test...syntactically. However it results in the
following error message:

{% highlight text %}
"True" does not equal "Details don't match"
{% endhighlight %}

Now, those error messages are there for a reason: for us to ignore them. Brian
and I spent at least five minutes scratching our head and stepping through the
test before we noticed the Equals. Sure enough, had we bothered to read the
message, it would've been only 30 seconds to our green bar.

