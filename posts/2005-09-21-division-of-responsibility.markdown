---
title: Division of Responsibility
---
So I was looking at a C# class that looked something like this:

~~~~ {.code}
class FooFactory
{
  private BazCollection _bazzen;
  private QuxCollection _quxxen;

  // ...constructors, other methds, etc...

  public Foo BuildFoo(Bar bar)
  {
    return new Foo(bar, _bazzen, _quxxen.FindQuxForBar(bar));
  }

  // ...more stuff...
}
~~~~

Now that smells to me. Take a moment and see if you can sniff it out. I'll
wait.

That's right. We're passing in `bar` and then also passing in something that
we use `bar` to get. But that's not all. I didn't show it in the snippet, but
inside Foo the way we use the BazCollection is _also_ indexed by `bar`. So, we
have this object and sometimes it indexes into a collection and sometimes the
objects that construct it index into a collection for it. The responsibilities
are muddled. Sometimes a Foo indexes in and sometimes it doesn't.

The power that objects give us is to wrap up data and the responsibilities
associated with that data into a nice little bundle. That power doesn't do us
a lot of good, though, if we don't actually use it. In a case like this the
responsibility of which object should be indexing into the collection is
spread onto two objects, and only should be in one.

Now, one refactor that we could do would be to just pass the collection in for
the Qux as well:

~~~~ {.code}
public Foo BuildFoo(Bar bar)
{
  return new Foo(bar, _bazzen, _quxxen);
}
~~~~

This refactor is not the one I would choose, though, as now it is even more
complicated to construct a Foo. Specifically if I want to test my Foo (which I
do, of course). The foo is only ever interested in a single Baz and a single
Qux, so it's just extra overhead to have to create a collection for each. That
brings me to the next refactoring that I might try:

~~~~ {.code}
public Foo BuildFoo(Bar bar)
{
  return new Foo(bar, _bazzen.FindBazForBar(bar), _quxxen.FindQuxForBar(bar));
}
~~~~

This has the advantage of making Foo's responsibility very clear. It is meant
to bring the bar, baz, and qux together. But, that call-site still stinks. In
fact, it reeks more with that original odor. At this point, it would be
worthwhile to see what Foo actually needs the Bar for and further factor that
out. Maybe we can have something like this:

~~~~ {.code}
public Foo BuildFoo(Bar bar)
{
  return new Foo(bar.name, bar.id, _bazzen.FindBazForBar(bar), _quxxen.FindQuxForBar(bar));
}
~~~~

That way we remove the dependence on Bar completely from the Foo, and push it
into the FooFactory. Maybe we will create an object that encapsulates those
parameters. In fact, maybe this factory is that object. But one thing is for
sure, this code is better separated and easier to test and use than the
original.

