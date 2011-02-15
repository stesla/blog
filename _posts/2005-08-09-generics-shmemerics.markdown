---
layout: post
wordpress_id: 22
title: Generics shmemerics
wordpress_url: http://www.alieniloquent.com/?p=22
---
It's no secret that the next version of Java will have generics, as will the
next version of C#. This is nothing incredibly new. C++ has had templates
since the dawn of time and they've always made code unreadable and needlessly
complex (okay, so I'm a dynamic language nut). It basically amounts to wanting
all the neat tricks that dynamic typing gives without any of the programmer
niceties (like trusting that the programmer knows what she's doing).

Over on [Blaine Buxton's blog][1] he posted about [Java generics][2] and the
_recommended_ naming for the generic types:

> A note on naming conventions. We recommend that you use pithy (single
character if possible) yet evocative names for formal type parameters. It’s
best to avoid lower 3 case characters in those names, making it easy to
distinguish formal type parameters from ordinary classes and interfaces. Many
container types use E, for element, as in the examples above.

{% highlight text %}

public interface List<E>

{

void add(E x);

Iterator<E> iterator();

}


public interface Iterator<E>

{

E next();

boolean hasNext();

}

{% endhighlight %}

In the comments over on Blaine's blog, this guy named Issac (who's mission it
appears to be to disagree with everything Blaine says) said:

> Everyone knows better than to use single letter names for classes and
interfaces - so when we see a single letter name, we can be confident the
parameter is not a class or an interface but is a formal type parameter.

Isaac clearly have a very narrow definition of "everyone" or a very narrow
sample of code to draw from. I see terrible naming in every language and it
makes code harder to read every time. I see the single letter and have to
spend the split second saying to myself "E, what a terrible class name, nobody
in their right mind would name a class that. Oh, right, that's the name of the
generic class."

The cognitive dissonance only becomes worse when you want to make a class
generic on more than one axis. When you have something like:

{% highlight text %}

public interface Matrix<E, F>

{

add(E x, F y);

//... more methods in E and F ...

}

{% endhighlight %}

When does it stop? How are we supposed to make "evocative" names when we only
have one letter to choose from? Sure, we can take the class name we would've
used and use the first letter, but then we are limited to only 26 names. It
might very well get confusing when we use E to mean Element in one place but
use it to mean Enumerator in another.

Now, if I actually give the thing a name, like ElementClass, I can say "Oh,
right, that's the class of whatever element is in here." Heck, I don't even
need to think that hard, it just _reads_ that way. It may seem like a small
win, but it's a huge win to somebody coming into the code from outside.

   [1]: http://www.blainebuxton.com/weblog

   [2]: http://www.blainebuxton.com/weblog/2005/08/huh.html
