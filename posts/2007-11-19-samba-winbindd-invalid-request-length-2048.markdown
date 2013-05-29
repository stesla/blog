---
title: "Samba winbindd invalid request length: 2048"
---
An update came down from [Gentoo][1] for Samba updating it to version 2.0.26a.
We had been having trouble with a problem in 3.0.24 that had been fixed in a
intervening release of Samba. So, naturally, I wanted to upgrade. But when I
did, I got this mysterious error in my `log.winbindd`.

~~~~ {.code}
[2007/11/19 13:27:16, 0] nsswitch/winbindd.c:request_len_recv(517)
  request_len_recv: Invalid request size received: 2084
~~~~

I have spent the entire day googling and yahooing and searching and grepping
to no avail. _Nothing_ I have tried worked. Now at one point I was reading and
somebody said "reboot, some other services are using stale references to
libnss_winbind.so." I couldn't imagine that was right, because if I rolled
back to 3.0.24 everything worked again.

Well, through a course of events I ended up with the following line in
`/etc/nsswitch.conf`:

~~~~ {.code}
shadow: shadow
~~~~

As you can imagine, that didn't work too well for my unix user that I keep on
the box for when ADS is hosed. So I broke out the trusty install CD, rebooted,
and fixed the file. I then rebooted and re-emerged samba. I _thought_ I had
put the mask in `/etc/portage/package.mask` to make sure it was 3.0.24 I was
installing, but I hadn't. Lo and behold everything worked.

All I had to do was reboot.

That was it.

   [1]: http://www.gentoo.org

