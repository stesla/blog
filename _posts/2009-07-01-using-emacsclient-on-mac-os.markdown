--- 
layout: post
wordpress_id: 161
title: Using <code>emacsclient</code> on Mac OS
wordpress_url: http://blog.alieniloquent.com/?p=161
--- |+
There's this feature of [TextMate][1] that a lot of people like: the `mate`
command. It opens up the file or directory you pass to it in your running
TextMate instance.

This sort of feature isn't anything new. Emacs has had this feature for a long
time. You just add this snippet to your to your `init.el`:

{% highlight text %}(emacs-server){% endhighlight %}

Then you can use `emacsclient` to send files to it the same way. However, on
my mac this wasn't working. The trick, it seems, is making sure you use the
_correct_ emacsclient.

About a year ago, [Emacs.app][2] was merged into the main-line GNU Emacs. So
you can build a completely Cocoa-based version of the very latest GNU Emacs
right out of CVS. If you do that, an `emacsclient` executable is included in
the application bundle.

You can write a little shell script, we'll call it `eopen`:

{% highlight text %}#!/bin/sh

# Send the file to our running Emacs and bring it to the foreground.

/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n "${1}" \

2> /dev/null \

&& open -a Emacs

# If that failed, then open a new emacs visiting the file.

if [ $? -ne 0 ]; then

EOPEN_DIR="${PWD}" EOPEN_FILE="${1}" open -a Emacs

fi{% endhighlight %}

Now, you'll notice that in the failure case, my script sets some environment
variables and just opens Emacs. Those environment variables aren't anything
special. In fact, unless you add some code to your `init.el` all you'll get is
a fresh Emacs open to whatever default buffer you usually see.

So, add the following to your `init.el`:

{% highlight text %}(let ((dir (getenv "EOPEN_DIR"))

(file (getenv "EOPEN_FILE")))

(if dir

(cd dir))

(if file

(find-file file))){% endhighlight %}

Then, when you have those environment variables set (such as in the shell
script), emacs will open up a buffer on the file or directory passed in as an
argument to the script. This hack is there so that you can use eopen to open
non-existent files in addition to existent files (`open` complains if the file
does not exist). Subsequent calls to the script can just use `emacsclient`.

   [1]: http://macromates.com/

   [2]: http://emacs-app.sourceforge.net/

