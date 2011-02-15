--- 
layout: post
wordpress_id: 93
title: Drawing graphs and automata in LaTeX
wordpress_url: http://blog.alieniloquent.com/2006/09/21/drawing-graphs-and-automata-in-latex/
--- "I am currently enrolled in a class entitled \"Automata, Computability, and\n\
Formal Languages\" at [UNO][1]. It's a neat class. The professor requires us to\n\
submit our homework in both hard and soft copies. Naturally, he suggests we\n\
use [LaTeX][2] to typeset our homework.\n\n\
Given that this is a class on automata, I end up drawing a lot of state\n\
diagrams to represent the machines I have to design for the problems. I wanted\n\
a nice way to draw these and put them in to my LaTeX document. The first thing\n\
I did was check to see if [OmniGraffle][3] exported to EPS, and it did. LaTeX\n\
can read EPS, so I was okay if I couldn't find anything better.\n\n\
With a solution in hand, I started to look and see if there was anything I\n\
could use to typeset these graphs right in the LaTeX source itself. Preferably\n\
something designed for drawing finite state-machines, even. What I found was\n\
[GasTeX][4]. It is a decently cool package.\n\n\
The following source code from their samples page:\n\n\
{% highlight text %}\n\n\
\\begin{figure}[H]\n\n\
\\begin{center}\n\n\
\\unitlength=4pt\n\n\
\\begin{picture}(15, 28)(0,-10)\n\n\
\\gasset{Nw=5,Nh=5,Nmr=2.5,curvedepth=3}\n\n\
\\thinlines\n\n\
\\node(A0)(7.5,0){$0$}\n\n\
\\node[Nmarks=i,iangle=90](A1)(0,13){$1$}\n\n\
\\node[Nmarks=if,iangle=90,fangle=90](A2)(15,13){$2$}\n\n\
\\drawloop[loopangle=180](A1){$a$}\n\n\
\\drawloop[loopangle=0](A2){$b$}\n\n\
\\drawedge(A1,A2){$a$}\n\n\
\\drawedge(A2,A1){$b$}\n\n\
\\drawedge(A2,A0){$a$}\n\n\
\\gasset{curvedepth=-3}\n\n\
\\drawedge[ELside=r](A1,A0){$b$}\n\n\
\\drawloop[loopangle=270](A0){$a, b$}\n\n\
\\end{picture}\n\n\
\\end{center}\n\n\
\\caption{La compl\xC3\xA9tion de l'automate.}\n\n\
\\end{figure}\n\n\
{% endhighlight %}\n\n\
Generates this graph (which I have scaled down):\n\n\
![Example of GasTeX output][5]\n\n\
One note about this snippet, though, is that I did not have to use the\n\
`center` environment to center the figures. Instead I used the `\\centering`\n\
directive inside my `figure` environment, and that worked nicely.\n\n\
It's nice to use and it makes pretty graphs. I'll be using it for the rest of\n\
the semester, that's for sure!\n\n   [1]: http://www.unomaha.edu\n\n   [2]: http://ctan.org/what_is_tex.html\n\n   [3]: http://www.omnigroup.com/applications/omnigraffle/\n\n   [4]: http://www.lsv.ens-cachan.fr/~gastin/gastex/gastex.html\n\n   [5]: http://blog.alieniloquent.com/images/gastex-example.jpg\n\n"
