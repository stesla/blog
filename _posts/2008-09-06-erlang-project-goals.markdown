--- 
layout: post
wordpress_id: 132
title: "Erlang Project: Goals"
wordpress_url: http://blog.alieniloquent.com/?p=132
--- |+
First off, I've decided on a name for my project: blogerl. I will be hosting
the source code [here][1] on GitHub.

With that bookkeeping out of the way, I'll get to the meat of the post. I want
to include you in my brainstorming process as I figure out what my goals are
with this project. In my next post I'll brainstorm features that will help me
meet these goals and select a subset of those features to implement initially.

Before I can start brainstorming features, I need to figure out an overall
vision for _what_ I am trying to build. So the first thing I want to describe
is the primary goal of this system, and possibly some secondary goals as well.

> I want a system that will manage the storage and presentation of my blog
content. That content will be primarily textual, and may be annotated with
various pieces of meta-data (e.g. date, title, or tags).

That's pretty vague, and I can design several extremely different systems that
will deliver on that, so I'm going to provide some additional goals, in
priority order, to narrow the design down.

Adding or editing content should be easy.

    So many blogging tools are a pain to use. I'll be honest, I'm not the
biggest fan of the web application. Especially not for things like writing. I
intend for Omniloquent to frequently have essays, and I don't really like
doing massive amounts of writing in a `<textarea>`.

Viewing the website should be fast.

    This isn't usually a problem in most modern blogging tools. However, I'm
writing this one from scratch, so it's going to be a little less modern at
first. I don't want people to be sitting around waiting for my content to
load. I want it to be snappy. The closer to static content it feels, the
happier I'll be.

Adding or editing content should be fast.

    The converse of the previous goal: I don't want posting to take forever.
This is why I hate Movable Type. The idea that I should have to rebuild dozens
of pages when I just update a single post is ridiculous. However, I am willing
to suffer a little, as I recognize that I'd rather it take me a second or two
to post but have the blog be lightning fast for my readers. That said, the
closer to instantaneous posting I can get, the happier I'll be.

   [1]: http://github.com/stesla/blogerl/tree/master

