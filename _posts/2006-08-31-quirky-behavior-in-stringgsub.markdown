---
title: Quirky Behavior in <code>String#gsub</code>
---
At my office I develop in Delphi. We use Delphi 2006. As far as IDEs go, it's
not that great. For example, when you tell the Delphi 2006 IDE to do a build
all (something you'd think developers do quite frequently), it has a very
annoying behavior: it eats up scads of memory. In fact when the build all
operation completes on our project group, Delphi has laid claim to over 1GB of
memory, and it won't let it go until you quit the application. But, this post
isn't about Delphi or its buggy IDE. It's about ruby. More specifically, it's
about a quirk (read: bug) in ruby.

The [`String`][1] class in ruby has a method called `gsub`. This method takes
two parameters and each can take two types of object. The first parameter can
either be a Regexp or a String, and it represents what is to be replaced. The
second can either be a String or a block, and supplies the value with which to
replace it. This seems perfectly natural.

Now, if you've ever used regular expressions, you probably know about back-
references. When you use the grouping operator in a regular expression (e.g.
`^a(ab)b$`) it stores a numbered back-reference to the matched value of each
group. In ruby you can reference these with the special variables `$1`, `$2`,
and so on. But, if you are passing a string as the replacement, it will only
be interpolated once and those back-references won't be correct. So, what
`gsub` does is let you put in `\1` and `\2` instead.

That behavior is awesome, and exactly what you want, if you're matching a
regular expression. But if you're just matching a string literal, there is
absolutely no reason to do it. In fact, if all you're doing is matching a
string literal those back-references will all be the empty string.

So, how do I know all this? Well, because Delphi 2006's build all operation
bites, we wrote a ruby script to replace it. This script has to do file-name
manipulation and all sorts of other string manipulation in order to get all of
the correct compiler options. One of the things it does is replace strings
like `$(CodeBase)` with a path such as `c:\svn\trunk`. Well, we have separate
code bases for our branches, and they have names like `c:\svn\2006`. You see
that `\2` there? Yeah, that one, right in the middle of the path. Even though
the script was matching a string literal, `gsub` was replacing back-
references. Since the path happened to have a `\2` in it, it would end up
coming out of gsub as `c:\svn006`, and that certainly wasn't right.

Thankfully, there is a simple work around. Instead of providing a string for
the replacement, we can provide a block. That block gets called every time and
the value that it returns is _exactly_ what gets used as the replacement.

   [1]: http://ruby-doc.org/core/classes/String.html

