---
layout: post
wordpress_id: 216
title: Fixing My Markup
wordpress_url: http://blog.alieniloquent.com/?p=216
---
Several years ago when I created the current design for this blog, I made a
rather unfortunate choice of markup for my code snippets. I decided to enclose
them in `<div>` tags like this:

{% highlight html %}
<div class="code"></div>
{% endhighlight %}

I'm really not sure _why_ I did that, although I have a vague recollection
that it had to do with my CSS not working otherwise.

There was an unfortunate consequence of that: I had to use `&nbsp;`s in order
to get indentation right inside the code blocks. Well, let me tell you, that
is a pain in the ass. Needless to say, it demotivated me from writing blog
articles with code snippets. This is a blog about programming. You can
practically hear the long silence that follows.

Well, I've been more and more motivated to blog stuff lately. However, I
wanted to fix this issue with my markup. It turns out that whatever issues I
had run into before are gone. This markup works:

{% highlight html %}
<pre class="code"></pre>
{% endhighlight %}

All I needed to do was go through my nearly 140 posts and change tags and
remove `&nbsp;`s. Yeah, that sounds like fun. Ruby to the rescue! After an
aborted attempt to use the AtomPub API (which gave a 302 to a 404), I spent a
little bit of time poring over the [api][1] [documentation][2]. Then I whipped
up this little script:

{% highlight ruby %}
require 'xmlrpc/client'

wp = XMLRPC::Client.new_from_uri('http://example.com/xmlrpc.php')
user = 'yeah'
pass = 'right'
blogid = 42

posts = wp.call("metaWeblog.getRecentPosts", blogid, user, pass)
posts.each do |post|
  newtext = post['description'].gsub(/<div class="code">(.*?)<\/div>/m) do
    "<pre class=\"code\">#{$1.gsub('&nbsp;',' ')}</pre>"
  end
  next if post['description'] == newtext
  post['description'] = newtext
  wp.call("metaWeblog.editPost", post['postid'], user, pass, post)
end
{% endhighlight %}

I ran it, held my tongue just right, and voila. Every post that had the old
markup now has the new markup. Just like that.

   [1]: http://codex.wordpress.org/XML-RPC_Support

   [2]: http://www.xmlrpc.com/metaWeblogApi

