--- 
wordpress_id: 216
layout: post
title: Fixing My Markup
wordpress_url: http://blog.alieniloquent.com/?p=216
---
Several years ago when I created the current design for this blog, I made a rather unfortunate choice of markup for my code snippets. I decided to enclose them in <code>&lt;div&gt;</code> tags like this:

<pre class="code">&lt;div class="code"&gt;&lt;/div&gt;</pre>

I'm really not sure <em>why</em> I did that, although I have a vague recollection that it had to do with my CSS not working otherwise.

There was an unfortunate consequence of that: I had to use <code>&amp;nbsp;</code>s in order to get indentation right inside the code blocks. Well, let me tell you, that is a pain in the ass. Needless to say, it demotivated me from writing blog articles with code snippets. This is a blog about programming. You can practically hear the long silence that follows.

Well, I've been more and more motivated to blog stuff lately. However, I wanted to fix this issue with my markup. It turns out that whatever issues I had run into before are gone. This markup works:

<pre class="code">&lt;pre class="code"&gt;&lt;/pre&gt;</pre>

All I needed to do was go through my nearly 140 posts and change tags and remove <code>&amp;nbsp;</code>s. Yeah, that sounds like fun. Ruby to the rescue! After an aborted attempt to use the AtomPub API (which gave a 302 to a 404), I spent a little bit of time poring over the <a href="http://codex.wordpress.org/XML-RPC_Support">api</a> <a href="http://www.xmlrpc.com/metaWeblogApi">documentation</a>. Then I whipped up this little script:

<pre class="code">require 'xmlrpc/client'

wp = XMLRPC::Client.new_from_uri('http://example.com/xmlrpc.php')
user = 'yeah'
pass = 'right'
blogid = 42

posts = wp.call("metaWeblog.getRecentPosts", blogid, user, pass)
posts.each do |post|
  newtext = post['description'].gsub(/&lt;div class="code"&gt;(.*?)&lt;\/div&gt;/m) do
    "&lt;pre class=\"code\"&gt;#{$1.gsub('&amp;nbsp;',' ')}&lt;/pre&gt;"
  end
  next if post['description'] == newtext
  post['description'] = newtext
  wp.call("metaWeblog.editPost", post['postid'], user, pass, post)
end</pre>

I ran it, held my tongue just right, and voila. Every post that had the old markup now has the new markup. Just like that.
