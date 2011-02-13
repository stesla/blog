--- 
wordpress_id: 105
layout: post
title: "Emacs on Mac: option key as meta"
wordpress_url: http://blog.alieniloquent.com/2007/08/16/emacs-on-mac-option-key-as-meta/
---
I'm an Emacs user, and I run on a Mac.  I just use GNU Carbon Emacs out of CVS.  

As long as I can remember, my command key mapped to the meta key in Emacs.  This was particularly bothersome as it got my fingers in the habit of typing command-w to yank some text.  But when I'm editing on a remote server in a Terminal window, that closes the window.  Sometimes I manage to do it twice before I hit the option key instead.

I used to have this in my <code>.emacs</code>, although I never really noticed it:

{% highlight text %}
(when stesla-mac-p
  (setq mac-command-key-is-meta nil))
{% endhighlight %}


A week ago when I wanted to finally fix this and make my command key do something other than be meta and make my option key be my meta key, I began to baffle as to why it wasn't that way already.  See, Google told me that the code I had in my <code>.emacs</code> should have done what I want.

Well, it turns out that it <em>used</em> to be how to do it.  Emacs is cooler now, and lets you specify the behavior of all three special keys.  So now what have is this:

{% highlight text %}
(when stesla-mac-p
  (setq mac-command-modifier nil)
  (setq mac-option-modifier 'meta))
{% endhighlight %}


This makes Emacs not recognize the command key as a modifier at all and use the option key as meta, which is how I like it.
