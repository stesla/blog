--- 
layout: post
wordpress_id: 81
title: Cocoa + Google = Yay!
wordpress_url: http://blog.alieniloquent.com/2006/08/23/cocoa-google-yay/
--- "So, I'm going to be writing an app to interface with [Google Calendar][1], so\n\
I need to learn how to speak the [Google Data APIs][2]. The first part of this\n\
is being able to authenticate using their client login protocol.\n\n\
I figured I'd throw together a simple little Cocoa app that just has text\n\
inputs for all the things that need to be sent and then I could do that.\xC2\xA0 I\n\
figured once I had the code written, I could extract it out into a class that\n\
I could use in my real app.\n\n\
I spent 30 minutes remembering how to throw the GUI together (I'm a bit\n\
rusty).\n\n\
I spent an hour or so understanding the NSURL API and figuring out how to [url\n\
encode][3] in Cocoa (it turns out there isn't anything in the Foundation\n\
classes).\n\n\
I spent another fifteen minutes not understanding why it told me my\n\
authentication was bad, despite it being correct, and then realizing I wasn't\n\
actually _sending_ my authentication information (or any part of the\n\
request).\xC2\xA0 It only took five minutes to fix it.\n\n\
I now get a successful response back from Google when I log in using my little\n\
login testing application.\xC2\xA0 How cool is that?\n\n   [1]: http://www.google.com/calendar/\n\n   [2]: http://code.google.com/apis/gdata/index.html\n\n   [3]: http://inessential.com/?comments=1&postid=1935\n\n"
