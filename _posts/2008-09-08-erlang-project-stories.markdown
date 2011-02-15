---
layout: post
wordpress_id: 133
title: "Erlang Project: Stories"
wordpress_url: http://blog.alieniloquent.com/?p=133
---
In my previous [post][1], I discussed the goals and priorities for blogerl.
This time I am going to brainstorm some stories and prioritize them. Then I'll
select the first few from the prioritized list to be my first iteration. The
goal is to have something I can deploy so I can get the blog started as soon
as possible.

My philosophy for stories is to give them titles that are short. The story
names are simply mnemonics to assist with recalling conversations and other
details. Since I'm a team of one, the only conversations will be those I have
here with you. But after this post, I'll probably just refer to the stories by
their title.

Here are the stories in priority order:

Storage

    I need some way to store my posts. I'm not very choosy about how this is
done. But, because I am in an experimental mood, I'm going to opt for using
[Git][2]. I'm probably going to end up using [Grit][3] via [erlectricity][4].
There's already been a [wiki][5] built on top of Git, why not a blog?

RESTful interface

    Like I said before, I don't want to compose my posts in a `<textarea>` but
I do need to be able to get content into my application somehow. So, I'm going
to design a RESTful interface to my data store. This is going to have to
include an authentication mechanism, since I don't want just anybody to be
able to update or edit my blog.

Emacs mode

    My preferred program for editing anything is Emacs. So it seems only
natural that I'd choose to write my first REST client as an emacs mode.

Index

    A front page that shows the posts in reverse chronological order. Should
be paginated with 10 posts to a page.

Single

    Permalinked pages for each individual post.

RSS

    Add an RSS feed for the main index.

Atom

    Add an Atom feed for the main index.

Archives

    Provide archives based on year and month.

Templates

    Templates for each of the views. Not YAWS pages. This will involve
creating my own template language. I don't like [ErlTL][6] for this as it is
too programmery.

Caching

    Keep rendered content in a cache with a TTL. Expire the cache for the
index page if a post is added, edited or deleted. Expire the cache for a
single post if it is edited or deleted.

Tags

    Be able to specify an arbitrary number of tags to associate with a post.
Provide archive pages for each tag.

Comments

    Provide comments. They do not need to be threaded. Plain, flat comments
are sufficient.

Trackbacks

    Provide the ability for other blogs to post trackbacks.

Post-dating

    Give the ability to submit a post that will be published at a later date.

Markdown

    Add support for formatting a post in the Markdown formatting language.

Textile

    Add support for formatting a post in the textile formatting language.

Formatting default

    Add a configuration option specifying the default formatting (which starts
out as plain HTML).

The first six are going to be what I aim to complete before I tag an 0.1 and
deploy to my webserver. That'll give me a means to share my blog posts with
people as a stream or individually via their browsers or via RSS feeds.

   [1]: http://blog.alieniloquent.com/2008/09/06/erlang-project-goals/

   [2]: http://git.or.cz/

   [3]: http://github.com/mojombo/grit/tree/master

   [4]: http://code.google.com/p/erlectricity/

   [5]: http://atonie.org/2008/02/git-wiki

   [6]: http://yarivsblog.com/articles/2006/10/17/introducting-erltl-a-simple-
erlang-template-language/

