---
title: "YAGNI: byte-order conversion explained"
---
In my [previous post][1] I talked about this problem I ran into with byte-
ordering and tests failing. Brian and Joe both felt that I left them hanging
by going into all the technical details of what my problem was and not going
into the technical details of why I shouldn't have used `htons` to start with.
Rather than editing the other post, here's a new one.

The code that I was using `htons` in was my SOCKS5 implementation. For those
who aren't familiar with SOCKS5, it is a proxying protocol. The computer
connects to the proxy and sends it a hostname and port for the actual
connection. The proxy then makes the connection and relays packets for the
remainder of the session.

Some of you may be guessing what I was calling `htons` on already: the port to
be serialized. Here are the two offending lines of code:

~~~~ {.code}
[buffer append:(0xFF00 & htons(port)) >> 8];
[buffer append:(0x00FF & htons(port))];
~~~~

That `buffer` was then, in turn, written out across the socket to the server
(or inspected by unit tests). But, since I was extracting each byte
individually the byte-order didn't matter since `0xFF00` will be in the same
byte order as `port` every time.

I hope this explains things a little better.

   [1]: /2006/08/29/yagni-byte-order-conversion.html

