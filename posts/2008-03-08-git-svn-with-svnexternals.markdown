---
title: <code>git-svn</code> with <code>svn:externals</code>
---
I've really fallen [head][1] over [heels][2] in love with Git. But my original
solution was really a hack. There is a better way to do it. In fact, it's so
much better, it comes with Git.

I took a look at [`git-svn`][3] when I was researching this a couple weeks
ago, and the trouble that I had was that it didn't fetch externals. Rather
than figure out the problem, I just moved on with the working solution that I
had. But, it bothered me. So I continued to research, and sure enough, Git has
a way to do it just fine.

Nazar Aziz over at [Panther Software][4] posted an excellent [guide][5] for
setting up a Rails app with plugins using `git-svn` and [Git submodules][6]. I
am going to distill it and add a few notes about how to make your use of Git
as unnoticeable to the other SVN users as possible.

#### Step One: Clone your externals

Git submodules are fantastic, but to use them you need Git repositories for
each of your externals. Fortunately, you can easily clone them with `git-svn`.
First, to list your externals:

~~~~ {.code}
$ svn propget svn:externals http://example.com/svn/app/vendor/plugins foo_plugin http://example.com/svn/foo_plugin/trunk
~~~~

Now you should make a directory to put your clones of these in.

~~~~ {.code}
$ mkdir ~/Projects/plugins
~~~~

And then cloning them is as simple as this:

~~~~ {.code}
$ git svn clone http://example.com/svn/app/foo_plugin/trunk ~/Projects/plugins/foo_plugin
~~~~

#### Step Two: Clone your SVN repository

The next step is to clone your repository sans-externals. We'll use `git-svn`
to do that, but we'll use it in a slightly different manner. The Git folks
recognize that there is a standard layout for SVN repositories. If you tell it
where the trunk, branches and tags are kept relative the the URI you provide,
it will try to preserve that information. It makes branches for each of the
SVN branches, and it makes branches for the tags as well.

Just do this wherever you want your project to live. You may want to rename
any SVN working copies you have so that there aren't any naming conflicts.

~~~~ {.code}
$ git svn clone http://example.com/svn/app -T trunk -t tags -b branches
~~~~

That'll give you a git repository named "app" in the current directory. The
master branch will be a remote tracking branch that is set up to track trunk,
and other branches are set up for any tags and branches.

#### Step Three: Hook up the submodules

Now that we've got Git repositories for all of the plugins, and a Git
repository for our project, we can hook everything up. We will set up a
submodule for the external we cloned above.

From within the top-level of your project repository do this:

~~~~ {.code}
$ git submodule add ~/Projects/plugins/foo_plugin vendor/plugins/foo_plugin
~~~~

After you've added the submodule do this:

~~~~ {.code}
$ git submodule init
$ git submodule update
~~~~

That should get the code from the plugin repository and into your project
repository just like the external did.

#### Step Four: Cover your tracks

When you are using `git-svn` it commits all of your Git commits into SVN, and
you don't really want to commit anything into your Git that you don't want
finding it's way into SVN (at least not on the branch that you commit to SVN
from). But it is easy to set Git up to ignore all of the files.

First, let's make sure Git ignores all the same things SVN was ignoring:

~~~~ {.code}
$ git svn show-ignore >> .git/info/exclude
~~~~

Then open up `.git/info/exclude` and add these lines to it:

~~~~ {.code}
# .git/info/exclude
.gitignore
.gitmodules
./vendor/plugins/foo_plugin
~~~~

That should prevent you from committing anything into SVN that is git-
specific.

#### Step Five: Using this thing

So once you're all set up, you'll want to be able to interact with the SVN
repository. Here are your two basic operations.

Update from SVN

~~~~ {.code}
$ git svn rebase
~~~~

This works just like `git-rebase`, except it pulls from SVN instead of some
other Git branch. It will not work if there are changes that have not been
committed to Git. What it does is roll back all of the changes since the last
time, and then update from SVN, then reapply the changes in order. If there
are conflicts, you resolve them as you would if you were using `git-rebase`.

Commit to SVN

~~~~ {.code}
$ git svn dcommit
~~~~

This will take all of the commits since your last time and commit them one at
a time to SVN. This allows all those people still using SVN to see each
individual commit instead of one monster commit.

I recommend using SVN to do anything more involved than simple adds, removes,
renames and edits.

Something to be aware of with this set up is that your submodules are
effectively frozen at whichever revision you cloned. If you want to update
them, you'll need to first update the cloned repository, and then run this
command at the root of your repository:

~~~~ {.code}
$ git submodule update
~~~~

Another caveat is that you need to keep your development as linear as you can.
Don't try to do anything crazy with lots of branches and merges between them.
SVN can't really make sense of it. The big deal here is you want to use `git-
rebase` to pull in changes from SVN.

Here's my workflow. I use a branch named work to do all of my work in. I will
sync it up with SVN several times a day, just so it isn't too stale. This is
how I do that:

~~~~ {.code}
$ git checkout master
$ git svn rebase
$ git checkout work
$ git rebase master
~~~~

Then, when I've commited all of my changes to my work branch, and I'm ready to
commit to SVN:

~~~~ {.code}
$ git checkout master
$ git merge work
$ git svn rebase # Just to be safe
$ git svn dcommit
~~~~

It works well, and it allows me to do my work disconnected from the network.

   [1]: /2008/02/20/svn-git-awesome.html

   [2]: /2008/02/22/svn-git-1-still-awesome.html

   [3]: http://www.kernel.org/pub/software/scm/git/docs/git-svn.html

   [4]: http://panthersoftware.com

   [5]: http://panthersoftware.com/articles/view/3/svn-s-svn-externals-to-git-s-submodule-for-rails-plugins

   [6]: http://www.kernel.org/pub/software/scm/git/docs/user-manual.html#submodules

