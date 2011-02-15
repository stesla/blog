---
layout: post
wordpress_id: 84
title: "Linker warning: -bind_at_load"
wordpress_url: http://blog.alieniloquent.com/2006/08/29/linker-warning-bind_at_load/
---
Apple has had Intel machines for about a year. When they came out with the new
architecture, they came up with the idea of a universal binary. It is a binary
that will run natively on either the PPC or Intel architectures. None of the
apps I work on have been compiled as universal binaries until today.

I just got my iMac yesterday, and it's beautiful. The most interesting thing
about it, though, is that it is an Intel machine. So naturally, when I got my
projects all set up on it, I finally had to bite the bullet and make things
work cross-platform.

That meant I had to recompile some frameworks to be universal binaries
themselves. I also had to twiddle some build options. It was all well
documented, and easy enough. But then I ran into a weird error that I couldn't
figure out:

{% highlight text %}/usr/bin/ld: warning suggest use of -bind_at_load, as lazy
binding may result in errors or different symbols being used

symbol _atan2f used from dynamic library

/Developer/SDKs/MacOSX10.4u.sdk/usr/lib/gcc/powerpc-apple-
darwin8/4.0.1/../../../libSystem.dylib(floating.o) not from earlier dynamic
library /usr/lib/libmx.A.dylib(single module){% endhighlight %}

After much searching, I finally found an [article][1], and it explained what I
needed to do. All I had to do was add `-lSystem` to my linking flags.

Now all I have to do is fix my one byte-order issue, and the program is all
better.

   [1]: http://lists.apple.com/archives/Cocoa-dev/2005/May/msg00474.html

