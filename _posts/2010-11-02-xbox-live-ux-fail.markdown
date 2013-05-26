---
title: XBox Live UX Fail
---
### The Story

Today I received an email from Microsoft. They wanted to let me know that in
about a month, my XBox Live Gold membership was going to automatically renew.
I really appreciate the notice because I don't own an XBox any more, so it was
time to cancel.

I clicked on the link provided in the email they sent me. Navigated the site
to the page with the link to manage my account billing, and clicked on the
link. "Oops, an error occurred." I navigated back, and tried something else.
"Oops, an error occurred." In fact, no matter what I tried to do on the site,
aside from look at the list of things I could (not) do caused an error to
occur.

Fine, I thought, I'll just call the phone number. I closed the window out of
frustration and picked up the phone. As I navigated the phone tree, I was
kindly reminded that I'd need details from my account, such as my gamertag.
Well shit, I don't remember that stuff, so I logged back in.

I was greeted with a page insisting I accept their new Terms of Service. I had
no way to proceed to the rest of the site without checking a box saying I'd
read them, and then clicking a button to proceed. So I did that, and a horde
of trained mice swarmed over their database to update the bit on my user
account record that said I'd agreed to the new terms.

Greeted, once again, with the page that offered to me all the things I could
do on their website, I clicked the link to manage my billing details. It
worked just fine. After hanging up the phone, I went through the multi-page
attempt to try and save me from cancellation ("But you won't be able to watch
Netflix on your XBox. Have you heard about these games? Maybe you want to go
month-to-month?") and turned off my auto-renewal setting.

### The Fail

There was actually two parts to the UX fail. The first was the lame, and at
one point seemingly-never-ending, attempt to save me from cancelling my auto-
renewal. I just wanted to turn off a setting, and I had to click through about
five pages of completely unrelated bullshit to get to the radio switch that
let me do that. The second part of the fail was a bit more subtle, and is
probably a bug in the XBox Live site.

When companies change their terms of service, they like to ensure that you
cannot do anything until you have accepted them. Products with a piece of
client software (e.g. EVE Online, iTunes) can simply disallow access until
you've clicked the appropriate widget. But websites are a bit more
complicated. The vendor cannot control your web surfing experience. All they
can control is what content they display to you. At first blush, that seems
like a simple problem, just redirect if the user hasn't accepted the TOS. But,
when your site has several possible paths a user can follow to enter, that
simple solution becomes vastly more complex.

My theory of what happened is that the link I clicked in my email went through
a route in their site that was _not_ wrapped in the code to see if I'd
accepted the new TOS. That allowed me access to my user portal. However, _all_
of the functionality was coded to verify I had accepted the TOS and (probably)
raise an exception if I had not. So, everything I tried to do gave me an
unexpected error. But, when I logged out, and came in through the _usual_
route, I was presented with the TOS, the flag was set, and everything worked.

