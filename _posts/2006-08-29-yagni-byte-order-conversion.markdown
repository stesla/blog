--- 
wordpress_id: 85
layout: post
title: "YAGNI: byte-order conversion"
wordpress_url: http://blog.alieniloquent.com/2006/08/29/yagni-byte-order-conversion/
---
So I have this project, and it compiles for the Mac on both PPC and i386 architectures.  Naturally, I have unit tests for this project.

One of the things that I've had to write for this is a simple SOCKS5 implementation (because of issues with Apple's implementation).  As part of this I had to do some manipulation of a port number and get the high and low bytes.

I've done this sort of thing before.  That's what <code>htons</code> and friends are for.  So naturally, I went ahead and put this in where I thought it mattered and went on my merry way.  On my ppc powerbook, my tests all passed.

Yesterday, when I ran the unit tests on my intel mac for the first time, I discovered that a test failed.  It was failing because of some byte-order problem.  After troubleshooting it, I narrowed it down to the <code>htons</code> calls.  It turns out I did not need them.

See the thing is, on a ppc, things are already in network byte-order so <code>htons</code> does absolutely nothing.  However, on i386 host byte-order is <em>different</em> from network byte-order.  So the reason it worked on ppc was that it was essentially as if I hadn't done it.  

It turns out that the particular thing I was doing didn't need the conversion to work correctly.  I removed it, and everything worked peachy on both machines.
