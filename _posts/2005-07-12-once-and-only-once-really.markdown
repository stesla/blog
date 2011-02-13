--- 
wordpress_id: 6
layout: post
title: Once and only once, really.
wordpress_url: http://blog.alieniloquent.com/2005/07/12/once-and-only-once-really/
---
The product that my company sells is very data intense.  The data is stored in a database.  Of course, from time to time we have to make schema changes to that database, so we have a program that does this for us.

In previous versions of the software most of this was done in Delphi code.  However, now that we run on a database that has good SQL support, we realized we could just run queries to get 99% of what we wanted done.  To take this a step further, we could just write a SQL script that does all the work and not have to recompile any code for the schema updater to do the right thing.

<pre class="code">
[2.1]
ALTER TABLE ...
<br />
[2.2]
DROP TABLE ...
CREATE TABLE ...
<br />
[2.7]
INSERT INTO ...
</pre>

You get the idea.  The program would then check the database to see if it was on the latest version and it would run any relevant SQL snippets on it in order to bring it up to date.  For each section it ran it would increment the version of the data to that version, run the SQL, and move on.  Of course, after it runs the last one that brings the data up to the last version.  Neato, right?  If only it were that simple.

The problem was that our code has a symbolic constant which is [supposed to] always be set to the latest data version.  The schema updater assumes that there will always be a section in the SQL script for the latest version and that will match the symbolic constant in the source code.  However, if the symbolic constant is incremented but the SQL script is not changed, the data is never brought up to the "latest" version, as the updater only increases the version number for each section in the script.

The "latest" version number is defined in two different places: the script and the constant.  The version is set to the value in the script, but validated against the constant.  This is the cause for the problem.  Now, I am biased toward the script being the One True Source, but in truth it only matters that there is only <em>One</em> True Source.

Either the schema updater should set the database version to the value used by the symbolic constant or it should check it against the last version in the script.  That way a single change will drive both pieces of functionality, and they'll always be in sync.
