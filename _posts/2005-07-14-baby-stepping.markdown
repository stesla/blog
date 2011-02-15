---
layout: post
wordpress_id: 8
title: Baby Stepping
wordpress_url: http://www.alieniloquent.com/?p=8
---
The product I work on is big. It has hundreds of thousands of lines of code.
There are some objects in that code that are used all over the place. A lot of
those objects are actually kept as global instances. This, of course, creates
all sorts of problems about data being synchronized and memory being freed.
The solution, of course, is to not use global objects.

While not using global objects is the solution to the general problem, it is
not easy in the least to make that change. There are hundreds of places where
these objects are used and each place is a little different. So, it would be
fairly costly to go in and fix them all, and it wouldn't show any visible
business value.

So how does one approach the problem? With baby steps. Each time I am in a
piece of code that uses one of these global references, I look at it to see if
I could pass in an object as a parameter to the method instead. If I can, then
I make that change. I just pass in the global at the call site, so it's still
using the global object. But, now that method has been severed from
_depending_ on the global object. If you conscientiously do this as you go
through the code, leaving it a little better every time, then eventually those
hundreds of references will become a handful. Once you only have a handful of
references, you probably don't need a global any more.

