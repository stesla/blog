---
layout: post
wordpress_id: 101
title: Base32 Encoded Freedom
wordpress_url: http://blog.alieniloquent.com/2007/06/05/base32-encoded-freedom/
---
So I'm writing the license-key generation code for the store-front for a
shareware program my friend Tyler and I are preparing to release (more about
that later). We've decided to use cryptography to reduce the likelihood that
our licensing schema will be compromised (for relatively little effort on our
part). We also decided to base32 encode the actual keys to make them easier to
read.

Well, the store-front is going to be a [Rails][1] app, of course. Ruby has a
module to [base64][2] encode, but it doesn't have one to base32 encode. So, I
wrote one, and I did it test first (of course).

The first four tests were easy. Really short strings, but they worked out most
of the kinks. But, I wanted something that would boost my confidence further.
So I wrote the following test which ended up being quite patriotic.

{% highlight text %}

def test_constitution_preamble

plaintext =<<-EOT

We the people of the United States, in order to form a more perfect union,

establish justice, insure domestic tranquility, provide for the common

defense, promote the general welfare, and secure the blessings of liberty

to ourselves and our posterity, do ordain and establish this Constitution

for the United States of America.

EOT

encoded = %W(

EAQCAIBAEBLWKIDUNBSSA4DFN5YGYZJAN5TCA5DIMUQFK3TJORSWIICTORQXIZLTFQQGS3RA

N5ZGIZLSEB2G6IDGN5ZG2IDBEBWW64TFEBYGK4TGMVRXIIDVNZUW63RMBIQCAIBAEAQGK43U

MFRGY2LTNAQGU5LTORUWGZJMEBUW443VOJSSAZDPNVSXG5DJMMQHI4TBNZYXK2LMNF2HSLBA

OBZG65TJMRSSAZTPOIQHI2DFEBRW63LNN5XAUIBAEAQCAIDEMVTGK3TTMUWCA4DSN5WW65DF

EB2GQZJAM5SW4ZLSMFWCA53FNRTGC4TFFQQGC3TEEBZWKY3VOJSSA5DIMUQGE3DFONZWS3TH

OMQG6ZRANRUWEZLSOR4QUIBAEAQCAIDUN4QG65LSONSWY5TFOMQGC3TEEBXXK4RAOBXXG5DF

OJUXI6JMEBSG6IDPOJSGC2LOEBQW4ZBAMVZXIYLCNRUXG2BAORUGS4ZAINXW443UNF2HK5DJ

N5XAUIBAEAQCAIDGN5ZCA5DIMUQFK3TJORSWIICTORQXIZLTEBXWMICBNVSXE2LDMEXAU===).join

assert_equal(encoded, Base32.encode(plaintext))

end

{% endhighlight %}

   [1]: http://www.rubyonrails.com

   [2]: http://ruby-doc.org/stdlib/libdoc/base64/rdoc/index.html

