---
title: Exposing .NET code in a DLL via C++
---
At my work, we do both Delphi for Win32 and .NET. Most of our .NET coding is
in C#, though, so we can't just cross-compile. We have a number of .NET
components that would be nice to use on the Win32 side (such as zip
libraries), and we know its supposed to be possible, but none of us have ever
gotten into the nitty gritty of actually doing it. Now, programming for
Windows isn't my favorite thing in the world. But, I do love a good technical
challenge. Especially one that requires me to dust off a language I haven't
used in a while. This will be the first of a few articles documenting the
things I'm learning from doing this.

The strategy that I'll be using is to write a DLL in C++ that will allow the
Delphi code to call into the .NET code. Microsoft's C++ allows you to mix
unmanaged and managed code fairly easily, so it makes for a very good glue
layer in situations like this.

So, to start off, let's make the Visual Studio project for the DLL. I'm using
Visual Studio 2005 for this, and I'll use the C++ CLR template entitled "Class
Library." I'll give it a name "Example" and hit go. This makes most of the
files that you need. Delete the class that it creates ("Class1") and make a
new one like this:

{% highlight c++ %}
// Example.h
#include <vcclr.h>

#pragma once

using namespace System;

namespace Example {
  public class Example1
  {
  public:

    Example1(const char * name)
      {
        _name = gcnew String(name);
      }

    ~Example1()
      {
      }

    void ShowName();

  private:

    gcroot<String ^> _name;
  };
}
{% endhighlight %}

A couple things to note about the above code. The `Example1` class is an
_unmanaged_ class, but the `_name` field is a managed object. The `gcroot`
class takes care of telling the garbage collector about the managed string
object.

Next, we need to export some DLL functions so that we can call them from
Delphi:

{% highlight c++ %}
// Exports.h
#define DLLAPI extern "C" __declspec(dllexport)
DLLAPI void * Example1Create(const char * name);
DLLAPI void Example1Delete(void * example);
DLLAPI void Example1ShowName(void * example);
{% endhighlight %}

The important thing to notice here is the `DLLAPI` define. You need to put it
before each function you want to export. We have to extern the functions as
C-style functions so that their names don't get mangled in the symbol table.

{% highlight c++ %}
// Exports.cpp
#include "stdafx.h"
#include "Example.h"
#include "Exports.h"

using namespace Example;

#define E1(p) ((Example1 *) p)

DLLAPI void * Example1Create(const char * name)
{
  return new Example1(name);
}

DLLAPI void Example1Delete(void * example)
{
  delete example;
}

DLLAPI void Example1ShowName(void * example)
{
  E1(example)->ShowName();
}
{% endhighlight %}

Note the macro defined to do the cast. We only have to do the cast in one
place right now, but as you expose more methods on the object, you'll have to
cast for every one. The macro makes it easier.

So to summarize what we have so far. In the DLL we have an unmanaged class
which can access the managed classes in .NET. Then we expose functions which
can create instances, destroy them, and call methods on them. These return
pointers which the caller must manage. The pointers are passed into all of the
functions which invoke methods, and those functions cast and then call.

In my next post I'll show how to consume this DLL from Delphi.

