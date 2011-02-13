--- 
wordpress_id: 110
layout: post
title: "Samba winbindd invalid request length: 2048"
wordpress_url: http://blog.alieniloquent.com/2007/11/19/samba-winbindd-invalid-request-length-2048/
---
An update came down from <a href="http://www.gentoo.org">Gentoo</a> for Samba updating it to version 2.0.26a.  We had been having trouble with a problem in 3.0.24 that had been fixed in a intervening release of Samba.  So, naturally, I wanted to upgrade.  But when I did, I got this mysterious error in my <code>log.winbindd</code>.

{% highlight text %}[2007/11/19 13:27:16, 0] nsswitch/winbindd.c:request_len_recv(517)
  request_len_recv: Invalid request size received: 2084{% endhighlight %}

I have spent the entire day googling and yahooing and searching and grepping to no avail.  <em>Nothing</em> I have tried worked.  Now at one point I was reading and somebody said "reboot, some other services are using stale references to libnss_winbind.so."  I couldn't imagine that was right, because if I rolled back to 3.0.24 everything worked again.

Well, through a course of events I ended up with the following line in <code>/etc/nsswitch.conf</code>:

{% highlight text %}shadow: shadow{% endhighlight %}

As you can imagine, that didn't work too well for my unix user that I keep on the box for when ADS is hosed.  So I broke out the trusty install CD, rebooted, and fixed the file.  I then rebooted and re-emerged samba.  I <em>thought</em> I had put the mask in <code>/etc/portage/package.mask</code> to make sure it was 3.0.24 I was installing, but I hadn't.  Lo and behold everything worked.

All I had to do was reboot.

That was it.
