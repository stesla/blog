---
title: Code Survival
---
While [Brian][1] and I were pairing today, we also found an offensive bit of
code.

~~~~ {.code}
property P: Pointer read FP;
~~~~

This was a _public_ property on a class. It had no meaningful name, and was of
the least-specific type in the system. (I'm prefer dynamic languages, but if
you're _going_ to use a type system, _use_ it.)

Some quick grepping revealed that the property was never used, and deleting it
and doing a build all confirmed it. We felt much better.

But, simply deleting it was not enough. We had to find out who had put this
terrible code into our codebase. Using `svn blame` to track back the
revisions, we discovered it was put in on revision 3 back in 1996 when the
file was first added to Visual Source Safe.

We were no longer very surprised at the naming of the variable. The
department's naming standards were not the same ten years ago. What we _were_
surprised to discover is that the property (well, it was a public field back
then) wasn't ever used in revision 3.

The field had _never_ been used.

You know you have a big ball of mud when a public field named P survives for
ten years on a class that is a vital part of your system even though it is
never used anywhere in the code.

   [1]: http://blog.briankohrs.com/

