--- 
wordpress_id: 100
layout: post
title: Three little, two little, one little-endian
wordpress_url: http://blog.alieniloquent.com/2007/04/24/three-little-two-little-one-little-endian/
---
I recently found myself wanting a Cocoa class that represents a set of 8-bit bytes.  Cocoa has NSCharacterSet, but that is for <code>unichar</code>, not <code>uint8_t</code>.  So I wrote one.  It was easy enough, I gave it an array of <code>UINT8_MAX</code> booleans and said that if a particular element in the array was <code>YES</code> then that byte was in the set, and not if the element was <code>NO</code>.

Initially the class only knew how to answer questions of membership: is a byte in the set or  not?  But then I found a number of places where I was enumerating all possible values and testing for membership, so I figured adding a method that would return a <code>NSData</code> with just the bytes included in the set would be useful.

So I wrote this:

<pre class="code">
- (NSData *) dataValue
{
  NSMutableData *result = [NSMutableData data];
  for (unsigned i = 0; i &lt;= UINT8_MAX; ++i)
  {
    if (contains[i])
      [result appendBytes: &amp;i length: 1];
  }
  return result;
}
</pre>

I had unit tests that proved it worked, and they all passed, so I checked in.  All was good in the world.

Five days later, I flip open my laptop and decide to use the program this code is part of.  I always try to eat my own dog food, and I prefer the freshest dog food I can get.  So, whenever I want to use this application, I delete it, update from our Subversion repository, and build it.

Much to my surprise, when I built it on my laptop, some of those tests did not pass.  I was expecting the <code>NSData</code> returned from <code>-dataValue</code> to have certain bytes in it.  The <code>NSData</code> I actually got back did have the correct <em>number</em> of bytes, but they were all zeroes.

I banged my head against it for about twenty minutes, until I had a flash of insight.  My desktop machine at home is an iMac, and inside it is an Intel Core Duo processor.  My laptop is a PowerBook, and inside it is a Motorola G4 processor.  The Core Duo, like most other Intel processors, stores numbers in the little-endian format, whereas the G4 stores them in big-endian format.

<a href="http://en.wikipedia.org/wiki/Endianness">Endianess</a> is a computer topic that makes a lot of programmers' heads hurt.  Unfortunately, Cocoa programmers do have to think about this now.  Since  Apple switched from their old, big-endian, Motorola platform to their new, little-endian, Intel platform, applications that are meant to run on both have to be aware of byte-order issues.

Computers store data in bytes, which are eight bits long.  However, eight bits is only enough to store a number up to 255.  In order to store larger numbers, computers just concatenate bytes together.  A 16-bit number is comprised of two bytes, and a 32-bit number is comprised of four.  The endianess of a system determines what order those bytes are stored in.

When you read a decimal number like 4242, you read it from left to right.  The most significant digit is the left-most digit.  Similarly, when you read a binary number like 1000010010010, the most significant digit is the left-most digit.  If we divide that number into bytes, 00010000 10010010, the left-most byte is called the most significant byte, or the high-order byte.  The right-most byte is called the least significant byte, or the low-order byte.

A big-endian processor, like the G4, stores numbers exactly like you'd read them.  So if you read a 16-bit integer in big-endian order, the first byte you read is the high-order byte.  Now, if the number is less than 255, for example 42, you'll get this: 00000000 00101010.

A little-endian processor, like the Core Duo, stores numbers just the opposite of how you'd expect.  The first byte you read is the least significant byte, followed by the next most significant byte, and then so on.  So when we read our binary number in we'll get 10010010 00010000 instead of what we expected.  Now, if we look at that small number again, you'd get this: 00101010 00000000.

So, to bring this back to my bug.  The <code>unsigned</code> type is actually an unsigned 32-bit integer.  Since my code was manipulating a set of 8-bit numbers, every single number would fit into the low-order byte of that <code>unsigned</code>, thus leaving the other three bytes all zero.

The line of code where I do this:

<pre class="code">[data appendBytes: &amp;i length: 1]</pre>

Is a clever little trick I've used to avoid having to actually declare a one-byte array when I want to append just one byte.  It works great if <code>i</code> is actually an <code>uint8_t</code>.  It also works great if <code>i</code> is an <code>unsigned</code> and stored in little-endian format, since the first byte happens to be the byte I'm interested in.  However, on a big-endian processor, that will reference the most significant byte of the number instead, and since <code>i</code> never gets any bigger than <code>UINT8_MAX</code> (which is 11111111 in binary), that byte will always be zero.

So now the code looks like this:

<pre class="code">
- (NSData *) dataValue
{
  NSMutableData *result = [NSMutableData data];
  uint8_t byte[1];
  for (unsigned i = 0; i &lt;= UINT8_MAX; ++i)
  {
    if (contains[i])
    {
      byte[0] = i;
      [result appendBytes: byte length: 1];
    }
  }
  return result;
}
</pre>

The compiler knows to do the <em>correct</em> conversion between the 32-bit and 8-bit types when assigning from one to another, so the new code now works on both of my machines.

<small><b>Update:</b> The title is a joke that <a href="http://www.sperari.com">Erica</a> made up when I told her about this bug.  All blame for its terribleness should go to her, I just recognized how apropos it was for the post.</small>
