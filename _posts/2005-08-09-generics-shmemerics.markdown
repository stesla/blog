--- 
layout: post
wordpress_id: 22
title: Generics shmemerics
wordpress_url: http://www.alieniloquent.com/?p=22
--- "It's no secret that the next version of Java will have generics, as will the\n\
next version of C#. This is nothing incredibly new. C++ has had templates\n\
since the dawn of time and they've always made code unreadable and needlessly\n\
complex (okay, so I'm a dynamic language nut). It basically amounts to wanting\n\
all the neat tricks that dynamic typing gives without any of the programmer\n\
niceties (like trusting that the programmer knows what she's doing).\n\n\
Over on [Blaine Buxton's blog][1] he posted about [Java generics][2] and the\n\
_recommended_ naming for the generic types:\n\n\
> A note on naming conventions. We recommend that you use pithy (single\n\
character if possible) yet evocative names for formal type parameters. It\xE2\x80\x99s\n\
best to avoid lower 3 case characters in those names, making it easy to\n\
distinguish formal type parameters from ordinary classes and interfaces. Many\n\
container types use E, for element, as in the examples above.\n\n\
{% highlight text %}\n\n\
public interface List<E>\n\n\
{\n\n\
void add(E x);\n\n\
Iterator<E> iterator();\n\n\
}\n\n\n\
public interface Iterator<E>\n\n\
{\n\n\
E next();\n\n\
boolean hasNext();\n\n\
}\n\n\
{% endhighlight %}\n\n\
In the comments over on Blaine's blog, this guy named Issac (who's mission it\n\
appears to be to disagree with everything Blaine says) said:\n\n\
> Everyone knows better than to use single letter names for classes and\n\
interfaces - so when we see a single letter name, we can be confident the\n\
parameter is not a class or an interface but is a formal type parameter.\n\n\
Isaac clearly have a very narrow definition of \"everyone\" or a very narrow\n\
sample of code to draw from. I see terrible naming in every language and it\n\
makes code harder to read every time. I see the single letter and have to\n\
spend the split second saying to myself \"E, what a terrible class name, nobody\n\
in their right mind would name a class that. Oh, right, that's the name of the\n\
generic class.\"\n\n\
The cognitive dissonance only becomes worse when you want to make a class\n\
generic on more than one axis. When you have something like:\n\n\
{% highlight text %}\n\n\
public interface Matrix<E, F>\n\n\
{\n\n\
add(E x, F y);\n\n\
//... more methods in E and F ...\n\n\
}\n\n\
{% endhighlight %}\n\n\
When does it stop? How are we supposed to make \"evocative\" names when we only\n\
have one letter to choose from? Sure, we can take the class name we would've\n\
used and use the first letter, but then we are limited to only 26 names. It\n\
might very well get confusing when we use E to mean Element in one place but\n\
use it to mean Enumerator in another.\n\n\
Now, if I actually give the thing a name, like ElementClass, I can say \"Oh,\n\
right, that's the class of whatever element is in here.\" Heck, I don't even\n\
need to think that hard, it just _reads_ that way. It may seem like a small\n\
win, but it's a huge win to somebody coming into the code from outside.\n\n   [1]: http://www.blainebuxton.com/weblog\n\n   [2]: http://www.blainebuxton.com/weblog/2005/08/huh.html\n\n"
