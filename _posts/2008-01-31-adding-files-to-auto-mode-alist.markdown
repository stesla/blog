---
title: Adding files to <code>auto-mode-alist</code>
---
One of the things that I require from an editor is decent syntax highlighting.
Emacs provides me that, and it also provides me with awesome indentation (I've
grown addicted to hitting TAB and having my text just go to the right place
regardless of where my cursor is). Emacs has a mode for every occasion and
typically will load the right one based on the name of the file or its
content. Recently, mine stopped doing that.

At first it was annoying. I'd open a LaTeX file and it wouldn't change to
`latex-mode`, or I'd open a C file and it wouldn't go into `c-mode`, and worst
of all, I'd load Emacs lisp files and they wouldn't load in `emacs-lisp-mode`.
I would just switch the mode by hand, but my patience could only last for so
long.

Yesterday I finally took the time to find the trouble, shoot it, and replace
it with better code. In my `.emacs` I had this:

{% highlight text %}
(setq auto-mode-alist
      (append
       '(("\\.dtd$" . xml-mode)
         ("\\.xml$" . xml-mode)
         ("\\.yml$" . conf-mode)
         ("bash_profile$" . sh-mode)
         ("bashrc$" . sh-mode))
       auto-mode-alist))
{% endhighlight %}

I've seen that same code all over the internet, and it used to work just fine.
But for some reason, now it does something to my `auto-mode-alist` and makes
Emacs hate it for some reason. So I do some research. The recommended way to
put stuff in an alist like that is `ADD-TO-LIST`.

So the above code becomes this:

{% highlight text %}
(mapcar (lambda (mapping) (add-to-list 'auto-mode-alist mapping))
        '(("\\.dtd$" . xml-mode)
          ("\\.xml$" . xml-mode)
          ("\\.yml$" . conf-mode)
          ("bash_profile$" . sh-mode)
          ("bashrc$" . sh-mode)))
{% endhighlight %}

Putting that code in fixed my problems, and now all of my modes load
correctly.

