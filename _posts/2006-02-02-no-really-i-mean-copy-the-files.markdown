--- 
wordpress_id: 41
layout: post
title: No, really, I mean copy the files
wordpress_url: http://www.alieniloquent.com/2006/02/02/no-really-i-mean-copy-the-files/
---
A couple of weeks ago, my department got a replacement for our linux server (well, that's really giving the machine more credit than it deserves, it was hardly a  server).  Being the only guy here who really knows how to set up a linux server in any sort of efficient manner, I naturally took on the task of getting the new hardware up and running.  There were, naturally, kinks (there always are) in the process, but I got the machine up and going.  We got our wiki and our Subversion repository migrated over to it, and all was good. 

Or so we thought.  We came in the next morning and nobody could check out from the repository.  I combed through the Apache error logs and discovered that the repository didn't think that some revisions existed, so naturally it wouldn't let us check out code that had been last modified in that revision.  So I tried <code>svnadmin verify /var/svn/repo</code> and sure enough, it blew up.  It said that it couldn't find a file under <code>/var/svn/repo/db/revprops</code> &mdash; very odd.  When I copied the files from the old repository (with the server off, natch) <code>cp</code> had not given me any errors.  So, I went and peeked in the old repository, and the file was there.  I copied it over, and the verfiy moved on, but only to break on another revision, and then another, and then another.  At that point, I wrote a simple shell script to copy all the files that were in the one directory but not in the other, and in the end I think I copied a total of about 120 files. 

Once all the files were copied, everything worked peachy.  I'm just completely baffled as to why <code>cp</code> didn't copy files and also didn't give any errors.  I wonder if it has something to do with one of the filesystems being Samba and the other being a local hard drive.
