--- 
wordpress_id: 133
layout: post
title: "Erlang Project: Stories"
wordpress_url: http://blog.alieniloquent.com/?p=133
---
In my previous <a href="http://blog.alieniloquent.com/2008/09/06/erlang-project-goals/">post</a>, I discussed the goals and priorities for blogerl. This time I am going to brainstorm some stories and prioritize them. Then I'll select the first few from the prioritized list to be my first iteration. The goal is to have something I can deploy so I can get the blog started as soon as possible.

My philosophy for stories is to give them titles that are short. The story names are simply mnemonics to assist with recalling conversations and other details. Since I'm a team of one, the only conversations will be those I have here with you. But after this post, I'll probably just refer to the stories by their title.

Here are the stories in priority order:

<dl>
  <dt>Storage</dt>
  <dd>I need some way to store my posts. I'm not very choosy about how this is done. But, because I am in an experimental mood, I'm going to opt for using  <a href="http://git.or.cz/">Git</a>. I'm probably going to end up using <a href="http://github.com/mojombo/grit/tree/master">Grit</a> via <a href="http://code.google.com/p/erlectricity/">erlectricity</a>. There's already been a <a href="http://atonie.org/2008/02/git-wiki">wiki</a> built on top of Git, why not a blog?</dd>

  <dt>RESTful interface</dt>
  <dd>Like I said before, I don't want to compose my posts in a <code>&lt;textarea&gt;</code> but I do need to be able to get content into my application somehow. So, I'm going to design a RESTful interface to my data store. This is going to have to include an authentication mechanism, since I don't want just anybody to be able to update or edit my blog.</dd>

  <dt>Emacs mode</dt>
  <dd>My preferred program for editing anything is Emacs. So it seems only natural that I'd choose to write my first REST client as an emacs mode.</dd>

  <dt>Index</dt>
  <dd>A front page that shows the posts in reverse chronological order. Should be paginated with 10 posts to a page.</dd>

  <dt>Single</dt>
  <dd>Permalinked pages for each individual post.</dd>

  <dt>RSS</dt>
  <dd>Add an RSS feed for the main index.</dd>

  <dt>Atom</dt>
  <dd>Add an Atom feed for the main index.</dd>

  <dt>Archives</dt>
  <dd>Provide archives based on year and month.</dd>

  <dt>Templates</dt>
  <dd>Templates for each of the views. Not YAWS pages. This will involve creating my own template language. I don't like <a href="http://yarivsblog.com/articles/2006/10/17/introducting-erltl-a-simple-erlang-template-language/">ErlTL</a> for this as it is too programmery.</dd>

  <dt>Caching</dt>
  <dd>Keep rendered content in a cache with a TTL. Expire the cache for the index page if a post is added, edited or deleted. Expire the cache for a single post if it is edited or deleted.</dd>

  <dt>Tags</dt>
  <dd>Be able to specify an arbitrary number of tags to associate with a post. Provide archive pages for each tag.</dd>

  <dt>Comments</dt>
  <dd>Provide comments. They do not need to be threaded. Plain, flat comments are sufficient.</dd>

  <dt>Trackbacks</dt>
  <dd>Provide the ability for other blogs to post trackbacks.</dd>

  <dt>Post-dating</dt>
  <dd>Give the ability to submit a post that will be published at a later date.</dd>

  <dt>Markdown</dt>
  <dd>Add support for formatting a post in the Markdown formatting language.</dd>

  <dt>Textile</dt>
  <dd>Add support for formatting a post in the textile formatting language.</dd>

  <dt>Formatting default</dt>
  <dd>Add a configuration option specifying the default formatting (which starts out as plain HTML).</dd>
</dl>

The first six are going to be what I aim to complete before I tag an 0.1 and deploy to my webserver. That'll give me a means to share my blog posts with people as a stream or individually via their browsers or via RSS feeds.
