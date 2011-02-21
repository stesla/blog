---
layout: post
wordpress_id: 124
title: SVN + Git = Awesome
wordpress_url: http://blog.alieniloquent.com/2008/02/20/svn-git-awesome/
---
I've been a big fan of [SVN][1] for several years now. I even helped my former
employer [migrate][2] from VSS a couple of years ago after I sold everybody on
the idea. I have lots of love for SVN, but it has its limitations, especially
the need to have network connectivity to a central repository. I know at least
[some][3] [people][4] would love to have a way to still commit code when
offline. So, here's how I did it with SVN and [Git][5].

Git is a DVCS that works differently than SVN. Instead of making changes in
your working copy and submitting them to a central repository, your working
copy _is_ your repository. You can push changes to another repository or you
can pull changes from another repository. It's a nice way of working, and it's
what they use on the Linux kernel. The Git folks have a nice [tutorial][6] for
SVN users.

The key feature of Git that makes it well suited for use alongside SVN is that
it keeps _all_ of its metadata in one folder at the top of your repository. It
does not put one in each directory like SVN does. So you can make your SVN
working copy into a Git repository and then ignore the folder and SVN knows
nothing about it. Here's what you do.

At the top level of your SVN working copy:

{% highlight text %}
$ git init
$ echo .svn > .gitignore
$ git add *
$ git add .gitignore
$ git commit -m "Initial commit"
{% endhighlight %}

Now we just need to teach SVN to ignore the Git stuff. So open up your
`~/.subversion/config` file and find the `[miscellany]` section. You should
see a commented out setting for `global-ignores`. Uncomment it and add `.git*`
to it like this:

{% highlight text %}
[miscellany]
### Set global-ignores to a set of whitespace-delimited globs
### which Subversion will ignore in its 'status' output, and
### while importing or adding files and directories.

global-ignores = *.o *.lo *.la \#*\# .*.rej *.rej .*~ *~ .\#* .DS_Store .git*
{% endhighlight %}

And voila! When you're able to connect to your SVN repo, you can use SVN. But
when you're offline and still want the ability to use version control to
incrementally save your changes, you can use Git. They're working on the same
files, so they play together very nicely.

   [1]: http://subversion.tigris.org

   [2]: http://blog.alieniloquent.com/2005/10/26/now-powered-by-subversion/

   [3]: http://blog.excastle.com

   [4]: http://blog.briankohrs.com

   [5]: http://git.or.cz/

   [6]: http://git.or.cz/course/svn.html

