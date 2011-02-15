--- 
layout: post
wordpress_id: 75
title: Extract Comments
wordpress_url: http://blog.alieniloquent.com/2006/08/15/extract-comments/
--- |+
So [Brian][1] and I were pairing today, and we were refactoring some legacy
code.  The method we were looking at was over 200 lines long, and it had
comments sprinkled throughout to make it semi-readable.

We highlighted a huge block of this method and hit the magic keystrokes to
Extract Method using Delphi 2006's automated refactoring tools.  It extracted
all of the code just fine, but it also removed some of the comments.

Note that I say _some_ of the comments were removed.  Some of them were not. 
There was no pattern that I could discern as to which comments were removed or
which were left alone.  It's almost as if the refactoring tools read the name
of the new methodl, read the content of the comment and said, "Nah, you don't
need the comment any more, the name is enough."

That's definitely not the kind of thing I want my computer doing for me.  Bad
Delphi!

   [1]: http://blog.briankohrs.com/
