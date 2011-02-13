--- 
wordpress_id: 125
layout: post
title: SVN + Git + 1 = Still Awesome
wordpress_url: http://blog.alieniloquent.com/2008/02/22/svn-git-1-still-awesome/
---
Yesterday I wrote about using <a href="http://blog.alieniloquent.com/2008/02/20/svn-git-awesome/">SVN and Git</a> together to have version control away from the network your SVN server is on. Now, I'll admit, I wrote that shortly after figuring it out and doing it. So, at that time, I hadn't actually come back into the office and merged my changes with the repository and committed them. Having done that a couple of times now, I'm here to say that this setup is fantastic.

So using this system you're either at the office, so you can use SVN, or you're not, so you have to use Git. I'll give steps to follow for each. You should probably read the documentation available from the <a href="http://git.or.cz/">Git</a> website to familiarize yourself further with these commands.

These steps assume that you've made a Git branch using the following command:

<pre class="code">$ git branch home</pre>

<h4>Taking your work home: SVN -> Git</h4>
Make sure your SVN working copy is as up to date as you want it. Ideally, commit any changes. But, if you're in the middle of a change set, that's fine.

<pre class="code">$ git commit -a -m "Merging in changes from SVN since last commit"
$ git checkout home
$ git merge master</pre>

Now you're ready to use Git to continue making your changes while you're away from the office.

<h4>Getting back to work: Git -> SVN</h4>
<pre class="code">Make sure your Git changes are all committed to the <code>home</code> branch.
$ git checkout master
$ svn up</pre>

Resolve any conflicts from SVN.

<pre class="code">$ git merge home</pre>

Resolve any conflicts from Git.

Now you can continue with your changes using SVN, or commit them right away if they're already perfect.
