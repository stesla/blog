---
layout: post
wordpress_id: 81
title: Cocoa + Google = Yay!
wordpress_url: http://blog.alieniloquent.com/2006/08/23/cocoa-google-yay/
---
    So, I'm going to be writing an app to interface with [Google Calendar][1], so
I need to learn how to speak the [Google Data APIs][2]. The first part of this
is being able to authenticate using their client login protocol.

I figured I'd throw together a simple little Cocoa app that just has text
inputs for all the things that need to be sent and then I could do that.  I
figured once I had the code written, I could extract it out into a class that
I could use in my real app.

I spent 30 minutes remembering how to throw the GUI together (I'm a bit
rusty).

I spent an hour or so understanding the NSURL API and figuring out how to [url
encode][3] in Cocoa (it turns out there isn't anything in the Foundation
classes).

I spent another fifteen minutes not understanding why it told me my
authentication was bad, despite it being correct, and then realizing I wasn't
actually _sending_ my authentication information (or any part of the
request).  It only took five minutes to fix it.

I now get a successful response back from Google when I log in using my little
login testing application.  How cool is that?

   [1]: http://www.google.com/calendar/

   [2]: http://code.google.com/apis/gdata/index.html

   [3]: http://inessential.com/?comments=1&postid=1935
