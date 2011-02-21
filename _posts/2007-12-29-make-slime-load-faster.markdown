---
layout: post
wordpress_id: 116
title: Make SLIME load faster
wordpress_url: http://blog.alieniloquent.com/2007/12/29/make-slime-load-faster/
---
I have joined up with some of the guys from [ODYNUG][1] who have started
meeting for breakfast and learning [Common Lisp][2] together. We are all using
some version of Emacs, [SLIME][3], and [SBCL][4].

[Blaine][5] shared a cool way to make SLIME load much faster by taking
advantage of the fact that Lisp uses images like Smalltalk (or more
accurately, Smalltalk uses images like Lisp). He posted it for the group to
see, but I wanted to post it here for my readers.

SBCL allows you to specify an image, or as they call it a core, by passing the
`--core` option along with (as far as I can tell) an absolute path to the core
file (well, at least it doesn't know that `~` means `$HOME`). It, of course,
also provides a way to create these core files, so you can load a bunch of
stuff in, and then save a core file that has all of that already loaded.

So first, go into your SLIME directory and copy `swank-loader.lisp` to `swank-
loader-original.lisp`. Then make `swank-loader.lisp` look like this (changing
`slime-dir` to be wherever your SLIME is, of course):

{% highlight text %}
(if (not (find-package 'swank-loader))
    ;; Edit SLIME-DIR to be where you have SLIME installed.
    (let ((slime-dir (merge-pathnames ".elisp/slime/" (user-homedir-pathname))))
      (load (merge-pathnames "swank-loader-original" slime-dir))))
{% endhighlight %}

Then, make a file called `bootstrap.lisp` with the following content:

{% highlight text %}
;; Load Swank
(load (merge-pathnames ".elisp/slime/swank-loader" (user-homedir-pathname)))

;; Save image
(sb-ext:save-lisp-and-die "sbcl-with-slime.core")
{% endhighlight %}

And run this command:

{% highlight text %}
$ sbcl --load bootstrap.lisp
{% endhighlight %}

Then copy `sbcl-with-slime.core` somewhere safe, I put mine in with my slime
code to keep it all together. Then you just have to add the following to your
`.emacs`:

{% highlight text %}
(let* ((slime-dir (concat elisp-dir "/slime"))
       (core-file (concat slime-dir "/sbcl-with-slime.core")))
  (setq inferior-lisp-program (concat "sbcl --core " core-file)))
{% endhighlight %}

Then you can `M-x slime` and it will be super fast.

   [1]: http://www.blainebuxton.com/odynug/

   [2]: http://en.wikipedia.org/wiki/Common_Lisp

   [3]: http://common-lisp.net/project/slime/

   [4]: http://sbcl.sourceforge.net/

   [5]: http://blainebuxton.com

