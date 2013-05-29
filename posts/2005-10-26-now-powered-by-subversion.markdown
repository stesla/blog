---
title: Now powered by Subversion
---
I [previously][1] [blogged][2] about Subversion at my workplace. I promised
that I would write a blog post about how the conversion went. Many of you may
think that I forgot. I didn't. We just finished it today, and as I expected, I
have things to share.

Saturday evening I kicked off [vss2svn][3] after installing all the things I
needed to run it. I had done this before on a small repository, but it was
long enough ago that I didn't remember exactly what it was like. So I started
it, and disconnected from VNC figuring it would take it a while. It did,
indeed, take it a while. In fact, it still hadn't done a _single thing_ when I
came in Monday morning. VSS has this need for a username to be specified. It
will use your windows login if it's a valid VSS user, but if it isn't, the
command line tool will prompt for one. The script ate that prompt, so I didn't
see it. That brings me to the first lesson.

**Lesson #1: Login to windows as a user with a VSS account.**

Monday, having guessed that was the problem, I fixed that by creating a VSS
account for the user I was running as. Problem solved. The script ran and it
built its database, and it told us what users we needed to have. I put all of
those users in my password file and hit enter. Boom! It blew up. It gave some
sort of error about not being able to import. After a good deal of time, it
was discerned that the script was not properly forming the import command.
It's a good thing I know Perl.

**Lesson #2: Download the perl script, not the executable, you _will_ need to
hack the source.**

Having fixed the script I launched the process. It ran again. This is the
point at which I start getting really annoyed at the fact that the script
doesn't pick things up where it left off. We are dealing with a very large
codebase here, so it takes a while to analyze. Thank goodness I was running
this on the machine where VSS and SVN are served and not over the network.

So it starts going. It converts. It converts some more. We get through two
hours and about 1500 revisions when, yes you guessed it, BOOM! It blows up.
This time it is asking for a revision of a file that VSS doesn't have. This is
somewhat odd, because you'd think that's what version control is for: to keep
old revisions. Well, you see, Microsoft decided to add a feature to VSS that
would allow you to keep all the revision history but only store the most
recent version of the file. So, if you analyze the history to figure out
commits and then ask for an old revision...you get nothing. Try to add a file
that doesn't exist to Subversion, and you get an error.

**Lesson #3: VSS has a lot of retarded \[mis\]features.**

Ruby to the rescue! We write a script to figure out how many of these files
exist. We run it. Wow, that's a lot of files. We look at all of them, and
determine that we don't care about their revision history anyway. So we'll
just "delete" them all from VSS and then re-add them. We've got a command line
tool. It can perform both operations. Excellent! We'll just write another Ruby
script. If only things were so simple.

Given that it's Microsoft -- the same company that said nobody would need more
than 640k of memory -- I don't find it surprising that our script didn't work.
I think somebody said "nobody will ever want to script VSS." You see, when you
"delete" a file from VSS, you don't actually delete the file. You simply set
the file aside in a pile of files that you don't play with any more, but it's
still there, even though you don't play anymore. If you try to add a file with
the same name in the VSS GUI, it will say "You used to play with this file,
would you like to just play with the file you used to have, or would you
rather play with this new file instead?" If you try the same thing using
`ss.exe` (the CLI), it simply says "You already have a file with that name,
sucker." Stupid program.

**Lesson #4: ss.exe sucks**

Once I readded all of those files by hand, and later discovered that ss.exe
put them somewhere else that was nowhere near where we asked it to, I kicked
off the script again. It ran, and ran, and ran, and ran. Then it ran and ran
and ran and ran. We started it Monday afternoon and it finished Wednesday
morning. It did, however, convert successfully.

**Lesson #5: 1,000,000ish lines of code and 8ish years of history = 2 days to
convert**

So, today we fixed up our buildscript (which was easyish) and our cruise
control, and we got everybody switched over. Our shop is now powered by
Subversion.

   [1]: /2005/10/20/subversive-joy.html

   [2]: /2005/10/20/subversion-apache-windows.html

   [3]: http://vss2svn.tigris.org/

