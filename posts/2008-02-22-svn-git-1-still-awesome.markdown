---
title: SVN + Git + 1 = Still Awesome
---
Yesterday I wrote about using [SVN and Git][1] together to have version
control away from the network your SVN server is on. Now, I'll admit, I wrote
that shortly after figuring it out and doing it. So, at that time, I hadn't
actually come back into the office and merged my changes with the repository
and committed them. Having done that a couple of times now, I'm here to say
that this setup is fantastic.

So using this system you're either at the office, so you can use SVN, or
you're not, so you have to use Git. I'll give steps to follow for each. You
should probably read the documentation available from the [Git][2] website to
familiarize yourself further with these commands.

These steps assume that you've made a Git branch using the following command:

~~~~ {.code}
$ git branch home
~~~~

#### Taking your work home: SVN -> Git

Make sure your SVN working copy is as up to date as you want it. Ideally,
commit any changes. But, if you're in the middle of a change set, that's fine.

~~~~ {.code}
$ git commit -a -m "Merging in changes from SVN since last commit"
$ git checkout home
$ git merge master
~~~~

Now you're ready to use Git to continue making your changes while you're away
from the office.

#### Getting back to work: Git -> SVN

Make sure your Git changes are all committed to the `home` branch.

~~~~ {.code}
$ git checkout master
$ svn up
~~~~

Resolve any conflicts from SVN.

~~~~ {.code}
$ git merge home
~~~~

Resolve any conflicts from Git.

Now you can continue with your changes using SVN, or commit them right away if
they're already perfect.

   [1]: http://blog.alieniloquent.com/2008/02/20/svn-git-awesome/

   [2]: http://git.or.cz/

