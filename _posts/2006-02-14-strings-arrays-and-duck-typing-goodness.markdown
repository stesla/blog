---
layout: post
wordpress_id: 42
title: Strings, arrays, and duck typing goodness
wordpress_url: http://www.alieniloquent.com/?p=42
---
We use more and more ruby around my shop every day, and that just tickles me
pink. One thing that we've been using it a lot for is managing our Subversion
working copies. We have a script that will delete unversioned files. We have a
script that will delete ignored files. I wanted to write a script that would
do both of those things and also revert any modified files (thus returning the
working copy to a pristine state, essentially).

There was a _lot_ of duplication between the two scripts. In fact, the only
thing different was one character in a regex: it was '?' for the unversioned
and 'I' for the ignored. I went through and wrote a new class to represent
these things and then I wanted to write a method named `delete_if_status`
which would take a list of statuses and delete any items in the checkout that
matched any of them.

I thought it would be cool if I could call it like this:

{% highlight ruby %}
list.delete_if_status(['?', 'I'])
{% endhighlight %}

But also call it like this:

{% highlight ruby %}
list.delete_if_status('?I')
{% endhighlight %}

Naturally, I figured Ruby would have a duck-typing answer to this problem, but
just the way it solved it surprised me (just a little--actually, now that I
think about it, it's unsurprising). Here is an IRB log that demonstrates just
what I discovered.

{% highlight text %}
irb(main):001:0> 'I?'.split
=> ["I?"]
irb(main):002:0> 'I?'.split('')
=> ["I", "?"]
irb(main):003:0> 'I?'.to_a
=> ["I?"]
irb(main):004:0> ['I','?'].to_s
=> "I?"
irb(main):005:0> ['I','?'].to_s.split('')
=> ["I", "?"]
{% endhighlight %}

So what I ended up with was this method:

{% highlight ruby %}
def delete_if_status(spec)
  status_list = spec.to_s.split('')
  self.delete_if do |item|
    status_list.include? item.status
  end
end
{% endhighlight %}

I love Ruby.

_Edited:_ Fixed some formatting with code and output snippets.

