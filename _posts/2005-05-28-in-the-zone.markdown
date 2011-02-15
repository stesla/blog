---
layout: post
wordpress_id: 3
title: In The Zone
wordpress_url: http://www.alieniloquent.com/?p=3
---
Tonight was amazing. I was amazing. Well, at least I amazed myself.

Before I tell you what amazed me, let me fill you in on the background. I've
been stalling on my latest project for a week now because I've had a mental
roadblock on how to refactor it to a place where I can add the next feature
Meg (my Customer) wants. I knew that the code was not in a place to add it,
but just could not find the path to salvation.

When I got home from work, I had some time to kill, so I pulled out my
powerbook and started refactoring. It was absolutely inspired. I just started
extracting little methods. I renamed a class. I renamed some methods on the
class. Just a handful of teensy little changes like that and all of the sudden
my code made so much more sense. Simply renaming the class and extracting a
couple of methods had revealed the design that I'd been looking for.

I went to dinner.

When we got back from dinner I set in to refactor more, and I've been
refactoring all night. It's been more of the same: teensy little changes. I'll
rename a method, or move it. I've removed three parameters from the
constructor of an object that had no real reason to ask for them, and that
made my code a lot prettier because I had to fish those parameters down
everywhere I constructed the object.

I think that the code is finally in a state that I can add the feature Meg
wants, but beyond that, it's in a much better state than it was this morning.

It's also worth noting that I've been doing this project test first, so I have
a safety net under me as I make all these broad changes. I've got a good test-
bed and that makes all of this possible.

