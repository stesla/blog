--- 
wordpress_id: 116
layout: post
title: Make SLIME load faster
wordpress_url: http://blog.alieniloquent.com/2007/12/29/make-slime-load-faster/
---
I have joined up with some of the guys from <a href="http://www.blainebuxton.com/odynug/">ODYNUG</a> who have started meeting for breakfast and learning <a href="http://en.wikipedia.org/wiki/Common_Lisp">Common Lisp</a> together. We are all using some version of Emacs, <a href="http://common-lisp.net/project/slime/">SLIME</a>, and <a href="http://sbcl.sourceforge.net/">SBCL</a>.

<a href="http://blainebuxton.com">Blaine</a> shared a cool way to make SLIME load much faster by taking advantage of the fact that Lisp uses images like Smalltalk (or more accurately, Smalltalk uses images like Lisp). He posted it for the group to see, but I wanted to post it here for my readers.

SBCL allows you to specify an image, or as they call it a core, by passing the <code>--core</code> option along with (as far as I can tell) an absolute path to the core file (well, at least it doesn't know that <code>~</code> means <code>$HOME</code>). It, of course, also provides a way to create these core files, so you can load a bunch of stuff in, and then save a core file that has all of that already loaded.

So first, go into your SLIME directory and copy <code>swank-loader.lisp</code> to <code>swank-loader-original.lisp</code>. Then make <code>swank-loader.lisp</code> look like this (changing <code>slime-dir</code> to be wherever your SLIME is, of course):

<pre class="code">
(if (not (find-package 'swank-loader))
    ;; Edit SLIME-DIR to be where you have SLIME installed.
    (let ((slime-dir (merge-pathnames ".elisp/slime/" (user-homedir-pathname))))
      (load (merge-pathnames "swank-loader-original" slime-dir))))
</pre>

Then, make a file called <code>bootstrap.lisp</code><code> with the following content:

<pre class="code">
;; Load Swank
(load (merge-pathnames ".elisp/slime/swank-loader" (user-homedir-pathname)))

;; Save image
(sb-ext:save-lisp-and-die "sbcl-with-slime.core")
</pre>

And run this command:

<pre class="code">
$ sbcl --load bootstrap.lisp
</pre>

Then copy </code><code>sbcl-with-slime.core</code> somewhere safe, I put mine in with my slime code to keep it all together.  Then you just have to add the following to your <code>.emacs</code><code>:

<pre class="code">
(let* ((slime-dir (concat elisp-dir "/slime"))
       (core-file (concat slime-dir "/sbcl-with-slime.core")))
  (setq inferior-lisp-program (concat "sbcl --core " core-file)))
</pre>

Then you can </code><code>M-x slime</code> and it will be super fast.
