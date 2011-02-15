--- 
layout: post
wordpress_id: 276
title: "Interview Question: Combinations"
wordpress_url: http://blog.alieniloquent.com/?p=276
--- "So I had a job interview yesterday with a great company, and I met a lot of\n\
awesome people. A question that was asked, presumably to test my approach to\n\
algorithm design, caught me off guard, and I didn't give the best answer I\n\
feel I was capable of. It bugged me the whole flight home, so, I whipped up a\n\
better answer. It seemed like the perfect kind of thing to post here on my\n\
blog.\n\n\
The problem: generate all combinations (without repetiton) with length `len`\n\
of the numbers from 1 to `max`.\n\n\
Hearing that problem, my mind immediately jumped to all of my combinatorics\n\
background, and I started thinking about how to _count_ all of those\n\
combinations. I really just could not get my mind out of that little rat hole,\n\
but my interviewers coaxed me out. By the time we were done with that part of\n\
the interview, we had come up with something that was on the right track, but\n\
still wouldn't work.\n\n\
A few simplifying assumptions can be made. Since these are combinations, order\n\
doesn't matter (e.g. `[1,2,3]` is the same as `[3,2,1]`). Also, since they do\n\
not have repetition, we can place an ordering constraint on the individual\n\
elements. So, for the 3-element combinations, we can say we want all\n\
combinations `[z,y,x]` where `1 \xE2\x89\xA4 x < y < z \xE2\x89\xA4 max` (the tuple is backwards for\n\
convenience of implementation, you could do it the other way just as easy).\n\n\
So with those two assumptions in mind, I stubbed out my function.\n\n\
{% highlight text %}\n\n\
combinations :: Int -> Int -> [[Int]]\n\n\
combinations len max = undefined\n\n\
{% endhighlight %}\n\n\
I had gotten three-quarters of the way toward a working implementation during\n\
my interview, so I was already leaning toward a recursive solution here. But,\n\
when I keyed in what I'd come up with earlier, it wasn't working right. I was\n\
getting things like `[2,2]` which should just not show up. I also could not do\n\
something like `combinations 3 3` and get anything back. I clearly had some\n\
boundary issues. So, I decided to actually write out the sets I was expecting\n\
and see if I saw any patterns.\n\n\
{% highlight text %}\n\n\
combinations 0 3 => [[]]\n\n\
combinations 1 3 => [[1],[2],[3]]\n\n\
combinations 2 3 => [[2,1],[3,1],[3,2]]\n\n\
combinations 3 3 => [[3,2,1]]\n\n\
combinations 4 3 => [[]]\n\n\
{% endhighlight %}\n\n\
The most obvious thing is that there's a clear relationship between the length\n\
of the combination and the number of combinations available, which is pretty\n\
basic combinatorics. There's only one way to choose 3 items from a set of 3\n\
items. But looking at this, I'm trying to conceive of some way to devise a\n\
recursive algorithm to produce those lists. So I rewrite the output to show\n\
how I would expect those to get built recursively.\n\n\
{% highlight text %}\n\n\
combinations 0 3 => [[]]\n\n\
combinations 1 3 => [1:[]] ++ [2:[]] ++ [3:[]]\n\n\
combinations 2 3 => [2:[1]] ++ [3:[1], 3:[2]]\n\n\
combinations 3 3 => [3:[2,1]]\n\n\
combinations 4 3 => [[]]\n\n\
{% endhighlight %}\n\n\
Now it might be apparent why I chose the ordering constraint I did. It makes\n\
it easy to build these lists with conses. The most imporant observation to\n\
make from this data is what numbers actually get selected to be consed. At\n\
each level of recursion we're selecting only the numbers between `len` and\n\
`max` to be added onto lists, and then we recurse with all the numbers _less\n\
than_ those.\n\n\
Here is the final implementation:\n\n\
{% highlight text %}\n\n\
combinations 0 _ = [[]]\n\n\
combinations len max = foldr reduce [] [len..max]\n\n\
where reduce x ys = recurse x ++ ys\n\n\
recurse x = prepend x (combinations (len - 1) (x - 1))\n\n\
prepend x = map (\\xs -> x:xs)\n\n\
{% endhighlight %}\n\n"
