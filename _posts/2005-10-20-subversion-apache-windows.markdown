--- 
layout: post
wordpress_id: 36
title: Subversion + Apache + Windows
wordpress_url: http://www.alieniloquent.com/?p=36
--- |+
I've always set up Subversion and Apache on Unix systems. Heck, aside from
work, I generally don't use Windows. However, at work, it's all Windows on our
production boxes. So, naturally, we needed our version control to run on a
Windows box.

I started out reading Brian's [guide][1] to installing everything. Now, I
didn't use his one-click installer, but I followed the rest. Notably, I
believe the system account on our server didn't pay attention to the path I
had set system-wide, as it could not load the Subversion modules unless I gave
it absolute paths for everything. But I got that working. It was interesting
to note that when I had Apache installed, the Subversion installer actually
offerred to install and configure the modules for me. I had Subversion access
through HTTP.

Since we were going to be keeping our source code in this, I wanted to make
sure it was secure. Only authorized people should be able to access the
repository. I don't want to administer a password database, and furthermore
nobody wants to remember two passwords, so I wanted to hook into our Windows
domain authentication. The way to do this is [mod_auth_sspi][2]. I spent about
thirty minutes tracking down that module, as apparently the original author no
longer publishes it. Furthermore, I could only find the source.

Finding the source was only a third of the battle, though. Getting it built
and installed each took another thirty each. The Makefile is not very generic.
You have to edit lots of paths to get things to work, and it doesn't even
define `$(RC)` even though it uses it. But, I finally got it to compile. Then
came the adventure of installing it. I copied the module to the right place
and added the right line to the config. Apache won't start. I run it by hand
from the command line, and it tells me it can't find the file, despite it
being right there next to all the others it can find just fine. I spent most
of thirty minutes googling, and finally realized that it could find _my_ file
just fine. It was a DLL that my module loaded it couldn't find. Namely,
`msvcr71.dll`. I copied that in, and everything worked hunky dory.

My last concern was backing up our repository. Currently we just do a disk
backup of our VSS repository. I don't think we've ever had to restore from one
of those, and I'm not entirely sure it would work. But, I do know that
straight disk copy out of an active Subversion is bad, since it's database
files. So, I go looking about how to back it up. There's a script that comes
in the source tree called `hot-backup.py`. I installed Python and tried to run
it. No luck. It wasn't written with Windows in mind. I had to spend most of an
hour poking and prodding it. It turns out that `os.spawnl` doesn't read from
the path environment (on purpose) but _does not_ need or want the path to the
executable quoted, but `os.popen3` does read from the environment and _does_
need and want the path to the executable quoted. Once I got that all figured
out, I put it in the scheduler and hopefully it will run tonight during the
wee hours backing up my nearly empty repository.

More soon about moving the source over. It's very exciting.

   [1]: http://blog.briankohrs.com/2005/09/20/guide-to-installing-the-
subverison-http-module-on-windows/

   [2]: http://www.deadbeef.com/index.php/2004/03/29/mod_auth_sspi

