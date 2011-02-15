--- 
layout: post
wordpress_id: 98
title: They Had Really Long Names
wordpress_url: http://blog.alieniloquent.com/2007/03/25/they-had-really-long-names/
--- |+
Two years ago when my employer adopted Extreme Programming, we began to write
automated tests for our code. My memory is fuzzy at this point, but as I
recall we wanted to be able to write tests and keep the source files anywhere
in our source tree, and then have our test runner automagically find them, or
something like that. So when we created it we called it
`MasterTestXMLReportApp` because it was cool enough to deserve such a long
name.

As time passed those tests got slower and slower, so we split them into two
suites. One that was meant to be fast and one that was meant to be slow. The
slow ones were run once a night. We called that suite
`MasterTestNightlyXMLReportApp` because it ran every night.

Fast forward to yesterday. The "nightly" tests haven't run nightly in a long
time. They just run continuously, albeit very slowly. I can never remember the
name of the projects, and neither can any of my teammates. So finally we got
fed up. We renamed them to `FastTests` and `SlowTests`.

We held a little memorial service, and gave them a little plot on our 0.04
acres of whiteboard, complete with a headstone.

![They had really long names][1]

   [1]: http://www.alieniloquent.com/images/riptests.jpg

