---
layout: post
wordpress_id: 43
title: Unit Testing of Cocoa Apps
wordpress_url: http://www.alieniloquent.com/?p=43
---
There's plenty of articles out there telling you how to use [OCUnit][1],
especially now that it ships with XCode 2.0. They're all excellent. You should
read them, especially [Apple's][2]. This article is simply about a little
trick that I've come up with to solve a minor annoyance I have with using
Apple's method of hooking up what they call a dependent test bundle.

A dependent test bundle is a really neat idea, actually. It uses your actual
application as a framework at link time and then uses some nifty magic to
launch the test bundle from inside your application and run all of the tests.
Why bother with all of that? It means you can keep your application code in
one target and your test code in another target -- completely separate. That's
a worthy goal.

The only thing I don't like about this, though, is that if you're developing a
GUI Cocoa application (and let's face it, most people are) then the process of
running the tests from within the application has the consequence of popping
up the GUI and letting it sit there until you quit manually. That's annoying.

So I came up with a solution. It's really nothing that fancy, but I thought
I'd share. I make a Cocoa Shell Tool target, and I name it something like
`stub`. I make a C file and name it `stub-main.m`. In that file I put the
following three lines:

{% highlight text %}
int main(int argc, char *argv[]) {
  return 0;
}
{% endhighlight %}

Then I add all of the application source files that my tests are going to need
to link to and make sure it all compiles. I just use that as the bundle loader
for my tests, and it's all good. No GUI popping up.

For an example of how to do this, you can look at the source for [OCFit][3].

   [1]: http://www.sente.ch/software/ocunit/

   [2]: http://developer.apple.com/documentation/DeveloperTools/Conceptual/UnitTesting/Articles/CreatingTests.html#//apple_ref/doc/uid/TP40002171-BBCBGHCJ

   [3]: http://ocfit.tigris.org/

