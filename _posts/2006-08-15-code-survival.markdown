--- 
wordpress_id: 74
layout: post
title: Code Survival
wordpress_url: http://blog.alieniloquent.com/2006/08/15/code-survival/
---
While <a href="http://blog.briankohrs.com/">Brian</a> and I were pairing today, we also found an offensive bit of code.

<pre class="code">property P: Pointer read FP;</pre>

This was a <em>public</em> property on a class.  It had no meaningful name, and was of the least-specific type in the system.  (I'm prefer dynamic languages, but if you're <em>going</em> to use a type system, <em>use</em> it.)

Some quick grepping revealed that the property was never used, and deleting it and doing a build all confirmed it.  We felt much better.

But, simply deleting it was not enough.  We had to find out who had put this terrible code into our codebase.  Using <code>svn blame</code> to track back the revisions, we discovered it was put in on revision 3 back in 1996 when the file was first added to Visual Source Safe.

We were no longer very surprised at the naming of the variable.  The department's naming standards were not the same ten years ago.  What we <em>were</em> surprised to discover is that the property (well, it was a public field back then) wasn't ever used in revision 3.

The field had <em>never</em> been used.

You know you have a big ball of mud when a public field named P survives for ten years on a class that is a vital part of your system even though it is never used anywhere in the code.
