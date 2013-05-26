---
title: Making Terminal Bearable
---
I've been using Macintosh computers for several years now, but I came to them
from the Unix world. There were several expectations I had from my terminal
emulator. Among them were that the key that says Alt on it should be my Meta
key for Emacs key-chords, and that the Page Up and Page Down keys should page
up and page down in my application. None of those behaviors are the default,
so fixing them is one of the first things I do when I get a Mac.

In Leopard these settings are all in the Settings section of Preferences
(Cmd-, or Terminal > Preferences...), but in previous versions they are in the
Window Settings screen (Terminal > Window Settings...).

First, to take care of the Alt key issue, go to the Keyboard section of the
settings. There is a checkbox toward the bottom that says "Use option key as
meta." Check it.

With that problem solved, all that is left are the paging keys. The default
behavior is to have Shift Page Up send Page Up to the terminal and Page Up and
do the same for Page Down. In the Keyboard Settings you should see a grid with
keystrokes on the left and text on the right. In many cases it will be an ANSI
escape sequence to send to the terminal, in some cases it will be a special
action. Here's what you want to set things to.

* `page down = send string to shell: \033[6~`

* `page up = send string to shell: \033[5~`

* `shift page down = scroll to next page in buffer`

* `shift page up = scroll to previous page in buffer`

That effectively just swaps the keys, so that you still have the ability to
scroll your Terminal buffer with the keyboard.

After I fix the keyboard, I like to fiddle with colors and other things, which
will all be in the same preferences dialogs. Then, after all the tweaks and
fixes, Terminal is ready to use.

