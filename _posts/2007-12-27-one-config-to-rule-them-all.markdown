--- 
wordpress_id: 115
layout: post
title: One config to rule them all
wordpress_url: http://blog.alieniloquent.com/2007/12/27/one-config-to-rule-them-all/
---
Yesterday I was reminded the importance of familiarity and comfort with my tools. Over the years I have developed a set of configurations that work for me. I have configurations for <a href="http://www.gnu.org/software/bash/">BASH</a> and I have configurations for <a href="http://www.gnu.org/software/emacs/">emacs</a> and they help me be productive. Yesterday I started configuring my new computer here at my new job (yes I got a new job) and I couldn't get to them because they were on my laptop at home.

Several years ago I had a system that involved keeping all of my config files in a <a href="http://subversion.tigris.org/">Subversion</a> repository and a shell script to make symlinks from the real locations to the ones in <code>~/.config</code>. I eventually stopped using it, mostly because it was a little clunky and hard to get set up on new machines. Last night I devised a similar system but tweaked a few things and it has made it so much better. 

The first thing I changed was the revision control tool.  I'm using <a href="http://darcs.net">darcs</a> as the version control. It is a distributed version control system and it is much simpler to use. To top it all off, it does not put a directory in each directory I add to my repository, it just puts one <code>_darcs</code> folder at the top level. To top it all off, it's written in Haskell, so it gets cool points for that.

The second thing I tweaked was that instead of using <a href="http://en.wikipedia.org/wiki/Symbolic_link">symbolic links</a> I'm using <a href="http://en.wikipedia.org/wiki/Hard_link">hard links</a>. This means that both <code>~/.bashrc</code> and <code>~/.config/home/.bashrc</code> are actually pointing to the same file on disk. So I can update the darcs repository and the linked files out in the rest of my home directory will get updated too, but if I delete the repository, I'll still have copies of the config.

Last, instead of keeping a flat list of files like <code>~/.config/bashrc</code> and <code>~/.config/ssh_config</code>, I'm keeping the files in a directory with their exact file names and the directory structure that they'd be stored in under my home directory. This makes writing the linking script much easier.

So with this structure in place I wrote an update script that makes directories and hard links so that what's in my home directory mirrors what's in my config repository. I even protected against files already existing with a friendly prompt (courtesy of <code>ln -i</code>).

A darcs repository of my config, including the update script, is available <a href="http://www.alieniloquent.com/code/config/">here</a>.
