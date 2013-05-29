---
title: "git svn fetch dies with \"fatal: bad object\""
---
At the day job, I use git-svn to interface with the SVN repository my team
uses to manage its code. For the last several months that's been going on
painlessly. For the last couple of weeks I haven't been doing anything in the
SVN repo, so I hadn't been fetching. This morning, I wanted to stash a script
into the repo, and so the first thing I did was `git svn fetch`. But, I was
greeted with a beastly error:

~~~~ {.code}
Index mismatch: c2714646b0290e3e3c44a0d5f239e1849083aefe != e12bd4a7d89970506864ad2b1c5caeef0a6a0876
rereading f2260561581e98397c66b939d90efb2a351ce993

...SNIP: a bunch of SVN output...

fatal: bad object 5b32d4ac2e03a566830f30a702523c68dbdf715b
rev-list -1 4a9fb23c88083656f6019fdedd7dc3d61b12ea50 85d1c926ebf06809247ae33850a94641f784bb66 3cd61c68706584009288ffc6e0c51d9dffdf9e5c 94b0c26b28610ea96e8989f4be21c92a9d78ef96 d82148570448de76f11f6b47b64ca3d4ac69cd21 de7e70d23ae10c9025b66206962b0fe358752268 dd02022bac0c2127300c937d80dfa793503c484c 36e6caf0aec8ae79d1d40cb4687466cd2f9e3718 5b32d4ac2e03a566830f30a702523c68dbdf715b 796a816059111a495c8ca14836c9bf567cc19c2a a0c450588867bbe11363d7dd6eb8f610b21c7f6c 4c7b780c0369a17e9f166aaeb851d9e14ecaeeea 56215cbb64f791cd85bf6094c18033f8712960ea 2ac18c2a0f144e64ff03ffab0e8ab6e7c3dc08b5 ee3fefa770a98ef90c9696c7a635ffda8d762404 971afb2b2dbacb83ef96f68b3b84960ae97a6833 93900609d9ec96def83a43940c2722123ec5f73c 67edd1ed8ad06c79e39cbb631696077856961a84 --not f2260561581e98397c66b939d90efb2a351ce993: command returned error: 128
~~~~

I tried it again, and got the same error. So, at least the behavior was
deterministic. Indeed, running this pared down command yielded the same
result:

~~~~ {.code}
$ git rev-list 5b32d4ac2e03a566830f30a702523c68dbdf715b
fatal: bad object 5b32d4ac2e03a566830f30a702523c68dbdf715b
~~~~

Stepping through the code in git-svn it became clear that the command that was
failing was part of the code that tries to reconcile `svn:mergeinfo`
properties with the git commit info that git-svn produces. So I began to
develop a theory. I had recently run a `git gc` and perhaps it had deleted a
commit that was only referenced from git-svn's information.

From inside the `.git` directory, I ran this:

~~~~ {.code}
$ find . -exec grep -Hin 5b32d4ac2e03a566830f30a702523c68dbdf715b {} \;
Binary file ./svn/.caches/lookup_svn_merge.db matches
Binary file ./svn/.caches/check_cherry_pick.db matches
~~~~

That pretty much confirmed my suspicion. I removed those files (they're
caches, right?), and then my fetch was able to proceed successfully.

