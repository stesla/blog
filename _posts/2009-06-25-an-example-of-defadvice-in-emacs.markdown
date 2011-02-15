---
layout: post
wordpress_id: 142
title: An example of <code>defadvice</code> in Emacs
wordpress_url: http://blog.alieniloquent.com/?p=142
---
For decades there's been a [war][1] between zealous users of various editors.
The truth is, though, that it is completely unfair to compare editors such as
vi, Textmate, or BBedit with Emacs. Why is it unfair? Emacs isn't an editor,
it is a programming environment. Today I want to share an example just how
powerful this distinction is.

It's better to compare Emacs to Smalltalk. It has a very small core written in
C, but almost all of the functionality you think of as Emacs is written in
Emacs Lisp. Emacs is really just a Lisp interpreter with a lot of primitives
built in for shuffling text around on the screen. This has a lot of awesome
implications for people who want to build tools for programming.

For example, some people like to have a window pane that shows them the file
structure of their project. Textmate does this. With [nav][2] you can have
that in Emacs. However, it always puts the navigation on the left-hand side of
the frame.

One of my co-workers wanted his nav to show up on the right-hand side. So I
started by typing `C-h k nav RET` and that brought up the help for the
function `nav` which puts the navigation buffer on the screen.

{% highlight text %}(defun nav ()

"Run nav-mode in a narrow window on the left side."

(interactive)

(if (nav-is-open)

(nav-quit)

(delete-other-windows)

(split-window-horizontally)

**(other-window 1)**

(ignore-errors (kill-buffer nav-buffer-name))

(pop-to-buffer nav-buffer-name nil)

(set-window-dedicated-p (selected-window) t)

(nav-mode)

(when nav-resize-frame-p

(nav-resize-frame)))){% endhighlight %}

A quick terminology note. In Emacs-speak, operating-system windows are called
_frames_. A frame can be split into multiple _windows_.

Doing the same for some of the other functions (`split-window-horizontally`,
`other-window`, `pop-to-buffer`), I was able to determine what this function
was doing. The call to `split-window-horizontally` leaves the right-hand
window active, so the call to `other-window` hops back to the left-hand window
(but only because all windows were deleted before the split). Then from there
on out, everything assumes that it's in that left-hand window and alters its
behavior.

So, all we need to do is delete the line I bolded above and it should work.
Because this is Lisp, I just copy the whole function into a buffer called
`*scratch*`, make my edit, and evaluate the code. So, now when I type `M-x nav
RET` I call my new function that does not call `other-window`. Sure enough, it
shows up on the right-hand side.

However, I do not want to duplicate all the rest of that code. Emacs has an
awesome facility to assist me, and it's called `defadvice`.

{% highlight text %};;;;;;;; To launch nav on left side: M-x nav RET

;;;;;;;; To launch nav on right side: C-u M-x nav RET

(defadvice other-window (around other-window-nop))

(defadvice nav (around prefix-nav)

(if current-prefix-arg

(ad-activate-regexp "other-window-nop"))

(unwind-protect

ad-do-it

(ad-deactivate-regexp "other-window-nop")))

(ad-activate-regexp "prefix-nav"){% endhighlight %}

This is essentially [monkey-patching][3] by another name. When I activate this
advice it gets called instead of the function it's advising, then at the point
I call `ad-do-it`, the original function (and any previously defined advice)
gets called. In the above code, I have two pieces of advice. One simply turns
`other-window` into a no-op, and the other turns that on and off, but only if
you use the universal prefix.

You can do this for any function in the system. Even the functions that get
called when you hit individual keys. That is why Emacs is so powerful. It's
not an editor, it's an environment.

   [1]: http://en.wikipedia.org/wiki/Editor_war

   [2]: http://code.google.com/p/emacs-nav/

   [3]: http://en.wikipedia.org/wiki/Monkey_patch

