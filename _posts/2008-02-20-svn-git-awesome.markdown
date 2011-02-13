--- 
wordpress_id: 124
layout: post
title: SVN + Git = Awesome
wordpress_url: http://blog.alieniloquent.com/2008/02/20/svn-git-awesome/
---
I've been a big fan of <a href="http://subversion.tigris.org">SVN</a> for several years now. I even helped my former employer <a href="http://blog.alieniloquent.com/2005/10/26/now-powered-by-subversion/">migrate</a> from <acronym title="Visual Source Safe">VSS</acronym> a couple of years ago after I sold everybody on the idea. I have lots of love for SVN, but it has its limitations, especially the need to have network connectivity to a central repository. I know at least <a href="http://blog.excastle.com">some</a> <a href="http://blog.briankohrs.com">people</a> would love to have a way to still commit code when offline. So, here's how I did it with SVN and <a href="http://git.or.cz/">Git</a>.

Git is a <acronym title="Distributed Version Control System">DVCS</acronym> that works differently than SVN. Instead of making changes in your working copy and submitting them to a central repository, your working copy <em>is</em> your repository. You can push changes to another repository or you can pull changes from another repository.  It's a nice way of working, and it's what they use on the Linux kernel. The Git folks have a nice <a href="http://git.or.cz/course/svn.html">tutorial</a> for SVN users.

The key feature of Git that makes it well suited for use alongside SVN is that it keeps <em>all</em> of its metadata in one folder at the top of your repository. It does not put one in each directory like SVN does. So you can make your SVN working copy into a Git repository and then ignore the folder and SVN knows nothing about it. Here's what you do.

At the top level of your SVN working copy:

{% highlight text %}$ git init
$ echo .svn &gt; .gitignore
$ git add *
$ git add .gitignore
$ git commit -m "Initial commit"{% endhighlight %}

Now we just need to teach SVN to ignore the Git stuff. So open up your <code>~/.subversion/config</code> file and find the <code>[miscellany]</code> section. You should see a commented out setting for <code>global-ignores</code>. Uncomment it and add <code>.git*</code> to it like this:

{% highlight text %}[miscellany]
### Set global-ignores to a set of whitespace-delimited globs
### which Subversion will ignore in its 'status' output, and
### while importing or adding files and directories.
global-ignores = *.o *.lo *.la \#*\# .*.rej *.rej .*~ *~ .\#* .DS_Store .git*{% endhighlight %}

And voila! When you're able to connect to your SVN repo, you can use SVN. But when you're offline and still want the ability to use version control to incrementally save your changes, you can use Git. They're working on the same files, so they play together very nicely.
