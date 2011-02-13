--- 
wordpress_id: 89
layout: post
title: Quirky Behavior in <code>String#gsub</code>
wordpress_url: http://blog.alieniloquent.com/2006/08/31/quirky-behavior-in-stringgsub/
---
At my office I develop in Delphi.  We use Delphi 2006.  As far as IDEs go, it's not that great.  For example, when you tell the Delphi 2006 IDE to do a build all (something you'd think developers do quite frequently), it has a very annoying behavior: it eats up scads of memory.  In fact when the build all operation completes on our project group, Delphi has laid claim to over 1GB of memory, and it won't let it go until you quit the application.  But, this post isn't about Delphi or its buggy IDE.  It's about ruby.  More specifically, it's about a quirk (read: bug) in ruby.

The <a href="http://ruby-doc.org/core/classes/String.html"><code>String</code></a> class in ruby has a method called <code>gsub</code>.  This method takes two parameters and each can take two types of object.  The first parameter can either be a Regexp or a String, and it represents what is to be replaced. The second can either be a String or a block, and supplies the value with which to replace it.  This seems perfectly natural.

Now, if you've ever used regular expressions, you probably know about back-references.  When you use the grouping operator in a regular expression (e.g. <code>^a(ab)b$</code>) it stores a numbered back-reference to the matched value of each group.  In ruby you can reference these with the special variables <code>$1</code>, <code>$2</code>, and so on.  But, if you are passing a string as the replacement, it will only be interpolated once and those back-references won't be correct.  So, what <code>gsub</code> does is let you put in <code>\1</code> and <code>\2</code> instead.

That behavior is awesome, and exactly what you want, if you're matching a regular expression.  But if you're just matching a string literal, there is absolutely no reason to do it.  In fact, if all you're doing is matching a string literal those back-references will all be the empty string.

So, how do I know all this?  Well, because Delphi 2006's build all operation bites, we wrote a ruby script to replace it.  This script has to do file-name manipulation and all sorts of other string manipulation in order to get all of the correct compiler options.  One of the things it does is replace strings like <code>$(CodeBase)</code> with a path such as <code>c:\svn\trunk</code>.  Well, we have separate code bases for our branches, and they have names like <code>c:\svn\2006</code>.  You see that <code>\2</code> there?  Yeah, that one, right in the middle of the path.  Even though the script was matching a string literal, <code>gsub</code> was replacing back-references.  Since the path happened to have a <code>\2</code> in it, it would end up coming out of gsub as <code>c:\svn006</code>, and that certainly wasn't right.

Thankfully, there is a simple work around.  Instead of providing a string for the replacement, we can provide a block.  That block gets called every time and the value that it returns is <em>exactly</em> what gets used as the replacement.
