---
layout: post
title: Now Powerd By Jekyll
---
I know I promised more blogging. I know it's been weeks. I *have* been
working on my blog, just behind the scenes.

I decided that if I was going to start blogging seriously, I did not
want to be doing it in a web form. I've been using
[WordPress](http://wordpress.org/) for a long time now, and the
biggest thing that has always bugged me were my options for actually
typing in my blog posts. Web forms suck for writing. I'd rather use
Emacs.

Now, I realize there's ways to use Emacs to post to WordPress. I'm not
interested. The second thing that bugged me about my old setup is the
database. This is a blog. Except for comments and track-backs, it is
essentially static content. There is no reason to bring a database
into this transaction.

So, I started considering doing something simple that would generate
my content statically. I could store it in a Git repository and push
it up to my server. That's when I remembered
[Jekyll](http://jekyllrb.com). It was pretty much exactly what I
wanted, and it's written in Ruby, so I'll be comfortable hacking on it
when the need arises.

The trouble was that Jekyll doesn't have a lot of built-in generation
options. It will generate a page for each post, and it will run any
extra files through its template expansion, but for things like
archive pages some extra code is needed. But, Jekyll provides a plugin
mechanism that is more than adequate to allow for extension.

As part of this migration, I've decided to remove the commenting and
track-backing functionality from my blog. Experience has shown that I
don't really get much in the way of comments, and I believe there are
better ways to facilitate discussion. If, later, I feel that I should
have comments, I'll probably use [Disqus](http://disqus.com).

I also made a little tweak so that only new posts should show up in
the RSS feed. Since my scheme for determining GUIDs is new, I didn't
want to spam people with a bunch of posts that are quite old. Also
quite easy with Jekyll.

Ultimately, I'm quite happy with my new blog setup. It will help me
blog more frequently.
