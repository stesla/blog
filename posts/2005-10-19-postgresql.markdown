---
title: PostgreSQL
---
So I'm working on a website for [Erica][1]. She wants some custom coding done
and some database goodness, so naturally she asked me to do it.

Being The Ruby Guy, I decided to do it in Rails. I also decided to use
[MySQL][2], as it is what I know. Well, I ran into some snags, and I still
don't know what was causing them. Suffice it to say, MySQL was just not
working. So, I decided to give [PostgreSQL][3] a try.

Now, I've always know that PostgreSQL had more features than MySQL. I've heard
that it's generally better than MySQL. Yet, I've always been afraid to try it.
It has been a completely irrational fear, of course, but fear nonetheless.
Well, since I found myself needing a different database, I bit the bullet.

It was about damn time. Building PostgreSQL from source was a breeze. I've
built MySQL from source, and it's a pain in the ass. Setting up my database
was also a breeze. It is also a pain in the ass for MySQL. So PostgreSQL has
two counts of not being a a pain in the ass, and MySQL has none.

Then it comes time to think about database users. I've always thought it was a
little silly that MySQL conflates granting privileges to users with creating
users. PostgreSQL doesn't do that. You create users and then you assign them
privileges, as it should be.

The command line shell for PostgreSQL also is easier to explore and learn than
the one for MySQL. The built-in help is nice, and the online manual is _much_
easier to read.

So, all told, I have a new favorite database.

   [1]: http://www.sperari.com

   [2]: http://www.mysql.com

   [3]: http://www.postgresql.org

