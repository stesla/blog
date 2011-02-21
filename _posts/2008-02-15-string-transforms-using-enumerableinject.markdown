---
layout: post
wordpress_id: 121
title: String transforms using <code>Enumerable#inject</code>
wordpress_url: http://blog.alieniloquent.com/2008/02/15/string-transforms-using-enumerableinject/
---
I love functional programming, and I love [Ruby][1]. One of the most awesome
things about Ruby is how much it borrows from the functional programming
mindset. One of the most powerful concepts that functional programming brings
to the table is higher-order functions. Ruby's `Enumerable` module is a great
example of how it embraces the idea of higher-order functions to abstract out
the various things you do with a collection and let you focus on the operation
for each item.

One of the most mysterious methods on `Enumerable` is `Enumerable#inject`. The
example that's always given is this:

{% highlight text %}
irb> [1, 2, 3, 4].inject(0) {|sum, i| sum + i}
10
{% endhighlight %}

That's fine, and usually makes sense. But when you try to branch out into more
esoteric uses of inject, it can get confusing. So I'm going to give an example
of accomplishing something useful with inject that you hopefully find useful.

I always find myself doing a sequence of substitutions on a string. For
example, when I implement a [Telnet][2] client, I like to normalize the line
endings I'm sending so that they're sane. I accomplish that by translating
"\r\n" to "\n", then translating "\r" to "\n", then translating "\n" to
"\r\n". It's a simple thing to do, and I could do it like this:

{% highlight ruby %}
string.gsub("\r\n", "\n").gsub("\r", "\n").gsub("\n", "\r\n")
{% endhighlight %}

But that's not very extensible. I'd like to apply this idea of a sequence of
substitutions in an abstract way so that I can do dynamically. And while I
could do something with `Object#send`, that's like cheating. This is where
inject comes to the rescue.

{% highlight ruby %}
def normalize_line_endings(string)
  transforms = [proc {|s| s.gsub("\r\n", "\n")},
                proc {|s| s.gsub("\r", "\n")},
                proc {|s| s.gsub("\n", "\r\n")}]
  transforms.inject(string) {|s, transform| transform.call(s)}
end
{% endhighlight %}

`Kernel#proc` (or `Kernel#lambda` if you prefer) is Ruby's way of making
higher-order functions. It returns a block which you can then call with an
argument. In the above code, I make an array of transforms that take a string
and return a string. The call to inject at the end is where the magic happens.
It calls the first transform with `string` which was provided as the argument
to inject. Then it calls the second transform with the result of the first,
and it calls the third transform with the result of the second. That list
could be as big as you want. It could even be dynamically generated.

That's nice, but it's still a a little verbose. I like to hide my use of
`Kernel#proc` behind a declarative interface when I'm doing this sort of thing
with it. So here's how we can rewrite the method.

{% highlight ruby %}
def transform(string, specifications = [])
  transforms = specifications.collect do |spec|
                 proc {|s| s.gsub(spec[:from], spec[:to])}
               end
  transforms.inject(string) {|s, transform| transform.call(s)}
end

def normalize_line_endings(string)
  transform(string, [{:from => "\r\n", :to => "\n"},
                     {:from => "\r", :to => "\n"},
                     {:from => "\n", :to => "\r\n"}])
end
{% endhighlight %}

Of course, at that point, we don't really need to create the procs. We can
just use inject right on the specifications array, so the final code I came up
with for this was:

{% highlight ruby %}
def transform(string, specifications = [])
  specifications.inject(string) do |s, spec|
    s.gsub(spec[:from], spec[:to])
  end
end

def normalize_line_endings(string)
  transform(string, [{:from => "\r\n", :to => "\n"},
                     {:from => "\r", :to => "\n"},
                     {:from => "\n", :to => "\r\n"}])
end
{% endhighlight %}

Now that can be used with any list of transformations. Those transformations
can be dynamically generated, and it's a very clean implementation. That is
the power of `Enumerable#inject`.

   [1]: http://www.ruby-lang.org

   [2]: http://tools.ietf.org/html/rfc854

