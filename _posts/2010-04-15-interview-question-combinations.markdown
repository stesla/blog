---
layout: post
wordpress_id: 276
title: "Interview Question: Combinations"
wordpress_url: http://blog.alieniloquent.com/?p=276
---
So I had a job interview yesterday with a great company, and I met a lot of
awesome people. A question that was asked, presumably to test my approach to
algorithm design, caught me off guard, and I didn't give the best answer I
feel I was capable of. It bugged me the whole flight home, so, I whipped up a
better answer. It seemed like the perfect kind of thing to post here on my
blog.

The problem: generate all combinations (without repetiton) with length `len`
of the numbers from 1 to `max`.

Hearing that problem, my mind immediately jumped to all of my combinatorics
background, and I started thinking about how to _count_ all of those
combinations. I really just could not get my mind out of that little rat hole,
but my interviewers coaxed me out. By the time we were done with that part of
the interview, we had come up with something that was on the right track, but
still wouldn't work.

A few simplifying assumptions can be made. Since these are combinations, order
doesn't matter (e.g. `[1,2,3]` is the same as `[3,2,1]`). Also, since they do
not have repetition, we can place an ordering constraint on the individual
elements. So, for the 3-element combinations, we can say we want all
combinations `[z,y,x]` where `1 ≤ x < y < z ≤ max` (the tuple is backwards for
convenience of implementation, you could do it the other way just as easy).

So with those two assumptions in mind, I stubbed out my function.

{% highlight text %}

combinations :: Int -> Int -> [[Int]]

combinations len max = undefined

{% endhighlight %}

I had gotten three-quarters of the way toward a working implementation during
my interview, so I was already leaning toward a recursive solution here. But,
when I keyed in what I'd come up with earlier, it wasn't working right. I was
getting things like `[2,2]` which should just not show up. I also could not do
something like `combinations 3 3` and get anything back. I clearly had some
boundary issues. So, I decided to actually write out the sets I was expecting
and see if I saw any patterns.

{% highlight text %}

combinations 0 3 => [[]]

combinations 1 3 => [[1],[2],[3]]

combinations 2 3 => [[2,1],[3,1],[3,2]]

combinations 3 3 => [[3,2,1]]

combinations 4 3 => [[]]

{% endhighlight %}

The most obvious thing is that there's a clear relationship between the length
of the combination and the number of combinations available, which is pretty
basic combinatorics. There's only one way to choose 3 items from a set of 3
items. But looking at this, I'm trying to conceive of some way to devise a
recursive algorithm to produce those lists. So I rewrite the output to show
how I would expect those to get built recursively.

{% highlight text %}

combinations 0 3 => [[]]

combinations 1 3 => [1:[]] ++ [2:[]] ++ [3:[]]

combinations 2 3 => [2:[1]] ++ [3:[1], 3:[2]]

combinations 3 3 => [3:[2,1]]

combinations 4 3 => [[]]

{% endhighlight %}

Now it might be apparent why I chose the ordering constraint I did. It makes
it easy to build these lists with conses. The most imporant observation to
make from this data is what numbers actually get selected to be consed. At
each level of recursion we're selecting only the numbers between `len` and
`max` to be added onto lists, and then we recurse with all the numbers _less
than_ those.

Here is the final implementation:

{% highlight text %}

combinations 0 _ = [[]]

combinations len max = foldr reduce [] [len..max]

where reduce x ys = recurse x ++ ys

recurse x = prepend x (combinations (len - 1) (x - 1))

prepend x = map (\xs -> x:xs)

{% endhighlight %}
So I had a job interview yesterday with a great company, and I met a lot of
awesome people. A question that was asked, presumably to test my approach to
algorithm design, caught me off guard, and I didn't give the best answer I
feel I was capable of. It bugged me the whole flight home, so, I whipped up a
better answer. It seemed like the perfect kind of thing to post here on my
blog.

The problem: generate all combinations (without repetiton) with length `len`
of the numbers from 1 to `max`.

Hearing that problem, my mind immediately jumped to all of my combinatorics
background, and I started thinking about how to _count_ all of those
combinations. I really just could not get my mind out of that little rat hole,
but my interviewers coaxed me out. By the time we were done with that part of
the interview, we had come up with something that was on the right track, but
still wouldn't work.

A few simplifying assumptions can be made. Since these are combinations, order
doesn't matter (e.g. `[1,2,3]` is the same as `[3,2,1]`). Also, since they do
not have repetition, we can place an ordering constraint on the individual
elements. So, for the 3-element combinations, we can say we want all
combinations `[z,y,x]` where `1 ≤ x < y < z ≤ max` (the tuple is backwards for
convenience of implementation, you could do it the other way just as easy).

So with those two assumptions in mind, I stubbed out my function.

{% highlight text %}

combinations :: Int -> Int -> [[Int]]

combinations len max = undefined

{% endhighlight %}

I had gotten three-quarters of the way toward a working implementation during
my interview, so I was already leaning toward a recursive solution here. But,
when I keyed in what I'd come up with earlier, it wasn't working right. I was
getting things like `[2,2]` which should just not show up. I also could not do
something like `combinations 3 3` and get anything back. I clearly had some
boundary issues. So, I decided to actually write out the sets I was expecting
and see if I saw any patterns.

{% highlight text %}

combinations 0 3 => [[]]

combinations 1 3 => [[1],[2],[3]]

combinations 2 3 => [[2,1],[3,1],[3,2]]

combinations 3 3 => [[3,2,1]]

combinations 4 3 => [[]]

{% endhighlight %}

The most obvious thing is that there's a clear relationship between the length
of the combination and the number of combinations available, which is pretty
basic combinatorics. There's only one way to choose 3 items from a set of 3
items. But looking at this, I'm trying to conceive of some way to devise a
recursive algorithm to produce those lists. So I rewrite the output to show
how I would expect those to get built recursively.

{% highlight text %}

combinations 0 3 => [[]]

combinations 1 3 => [1:[]] ++ [2:[]] ++ [3:[]]

combinations 2 3 => [2:[1]] ++ [3:[1], 3:[2]]

combinations 3 3 => [3:[2,1]]

combinations 4 3 => [[]]

{% endhighlight %}

Now it might be apparent why I chose the ordering constraint I did. It makes
it easy to build these lists with conses. The most imporant observation to
make from this data is what numbers actually get selected to be consed. At
each level of recursion we're selecting only the numbers between `len` and
`max` to be added onto lists, and then we recurse with all the numbers _less
than_ those.

Here is the final implementation:

{% highlight text %}

combinations 0 _ = [[]]

combinations len max = foldr reduce [] [len..max]

where reduce x ys = recurse x ++ ys

recurse x = prepend x (combinations (len - 1) (x - 1))

prepend x = map (\xs -> x:xs)

{% endhighlight %}
