---
title: For those about to base32
---
I've scribbled this down so many times, I thought you all might want to
benefit a little from my troubles.

~~~~ {.code}
|0000 0|000 11|11 111|1 2222| 2222 3|333 33|33 444|4 4444
|0000 0|111 11|22 222|3 3333| 4444 4|555 55|66 666|7 7777
~~~~

That is a chart to help understand how five octets turn into eight
base32-encoded bytes. It should be fairly self explanatory, but when I say
that, I'm always wrong.

The top row is the octets. I've each group of four represents four bits, and
the numbers represent which octet it is. The bottom row represents the
quintets, with the same numbering scheme. I spaced the digits the same, so you
can see how they match up. The pipes are nice visual borders.

