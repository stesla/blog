--- 
wordpress_id: 81
layout: post
title: Cocoa + Google = Yay!
wordpress_url: http://blog.alieniloquent.com/2006/08/23/cocoa-google-yay/
---
So, I'm going to be writing an app to interface with <a href="http://www.google.com/calendar/">Google Calendar</a>, so I need to learn how to speak the <a href="http://code.google.com/apis/gdata/index.html">Google Data APIs</a>.  The first part of this is being able to authenticate using their client login protocol.

I figured I'd throw together a simple little Cocoa app that just has text inputs for all the things that need to be sent and then I could do that.  I figured once I had the code written, I could extract it out into a class that I could use in my real app.

I spent 30 minutes remembering how to throw the GUI together (I'm a bit rusty).

I spent an hour or so understanding the NSURL API and figuring out how to <a href="http://inessential.com/?comments=1&postid=1935">url encode</a> in Cocoa (it turns out there isn't anything in the Foundation classes).

I spent another fifteen minutes not understanding why it told me my authentication was bad, despite it being correct, and then realizing I wasn't actually <em>sending</em> my authentication information (or any part of the request).  It only took five minutes to fix it.

I now get a successful response back from Google when I log in using my little login testing application.  How cool is that?
