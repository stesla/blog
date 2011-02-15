--- 
layout: post
wordpress_id: 75
title: Extract Comments
wordpress_url: http://blog.alieniloquent.com/2006/08/15/extract-comments/
--- "So [Brian][1] and I were pairing today, and we were refactoring some legacy\n\
code.\xC2\xA0 The method we were looking at was over 200 lines long, and it had\n\
comments sprinkled throughout to make it semi-readable.\n\n\
We highlighted a huge block of this method and hit the magic keystrokes to\n\
Extract Method using Delphi 2006's automated refactoring tools.\xC2\xA0 It extracted\n\
all of the code just fine, but it also removed some of the comments.\n\n\
Note that I say _some_ of the comments were removed.\xC2\xA0 Some of them were not.\xC2\xA0\n\
There was no pattern that I could discern as to which comments were removed or\n\
which were left alone.\xC2\xA0 It's almost as if the refactoring tools read the name\n\
of the new methodl, read the content of the comment and said, \"Nah, you don't\n\
need the comment any more, the name is enough.\"\n\n\
That's definitely not the kind of thing I want my computer doing for me.\xC2\xA0 Bad\n\
Delphi!\n\n   [1]: http://blog.briankohrs.com/\n\n"
