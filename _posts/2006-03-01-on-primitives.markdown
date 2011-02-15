---
layout: post
wordpress_id: 47
title: On Primitives
wordpress_url: http://www.alieniloquent.com/?p=47
---
Blaine has a [post][1] about how strings are so frequently abused in object
models. We heap extra meaning onto a string when we really should make a
class. The reason we should make a new class is that each class represents a
concept in our domain, and we can add behaviours and data to each class.
You'll be hard pressed to do that with a string. So, to stay agile it's better
to make new classes for new concepts.

In Delphi, there's a similarly abused syntax trick. It looks like this:

{% highlight text %}

type TMyEnum = (meOne, meTwo, meThree);

const MyEnumStrings: array[TMyEnum] of string = ('First', 'Second', 'Third');

{% endhighlight %}

Then, wherever you want to get a string associated with one of the enum
tokens, you can just index into that constant array _using the enum value_.

This trick isn't limited to arrays of strings, either. You could have arrays
of integers, doubles, characters, or even objects, and you can make as many of
these arrays as possible. So if you have some setting and there's ten values
that change with that setting, then you could have the setting be an
enumeration and have ten constant arrays indexed on that.

In a world of procedural programming, that's a neat trick, and it can help a
lot. But Delphi shed it's procedural shackles years ago, and now it has the
power of objects to help it out. Sadly, many programmers are tied to Delphi's
procedural roots, and can't get away from this enumeration-indexed array
trick.

What's so bad about the trick? For starters, you're making global data. Sure,
it's constant data, but it's global, which makes finding where the data is
coming from that much harder for somebody new to the code (or somebody reading
it months after they wrote it). Secondly, in order to vary behavior based on
the enumeration, you have to have gnarly conditionals that branch based on the
type code. This makes methods longer and harder to understand and follow.
Lastly, it makes the types more resistant to change, especially if they're
serialized as integers: any change in order will change the ordinal (integer)
value of the same enumerated value.

What's the alternative? Make a class. You can easily make a class that
represents the type code, and you can make subclasses for each type. The class
can have methods for each of the constant arrays and the subclasses can
implement them to return the proper ordering. If there's behavior specific to
one type you can put it on this class hierarchy and polymorphically switch out
behaviors, instead of making gnarly conditionals. Moreover, there's no
inherent ordering from one class to the next, so you can completely control
how things are serialized and compared.

As Joshua Kerievsky says in his book [Refactoring to Patterns][2], so many
developers have an obsession with using language primitives such as strings,
integers, or enumerations. Whether that's because they think that it's too
much work to make a class, or it'll take too many CPU cycles, or it'll eat up
too much memory, it doesn't matter. The obsession with primitives is unhealthy
and it leads to spaghetti code that is hard to refactor.

We must resist the temptation and make supple, agile code. That means we must
make classes.

   [1]: http://www.blainebuxton.com/weblog/2006/02/string-how-i-loathe-thee.html

   [2]: http://www.amazon.com/gp/product/0321213351/

