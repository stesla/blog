--- 
wordpress_id: 109
layout: post
title: Remember ML-Yacc makes error-correcting parsers
wordpress_url: http://blog.alieniloquent.com/2007/10/09/remember-ml-yacc-makes-error-correcting-parsers/
---
So I got my language <a href="http://blog.alieniloquent.com/2007/10/08/samuels-lambda-and-the-y-combinator/">working</a>.  But there are still some things I want to add to it.  One thing that was bothering me was that both this code:

<pre class="code">fn x =&gt; x</pre>

and this code:

<pre class="code">x =&gt; x</pre>

parsed to the <em>same thing</em>.

I banged my head against this.  My grammar had the production right:

<pre class="code">expr : ...
     | FN ident RARROW expr (T.FnDef (ident,expr))
     ...</pre>

and my lexer produced the tokens just fine:

<pre class="code">&lt;INITIAL&gt; "fn" =&gt; (Tokens.FN(!pos, !pos));
&lt;INITIAL&gt; "=>" =&gt; (Tokens.RARROW(!pos, !pos));</pre>

So, I was confused.  I downloaded the source for <a href="http://smlnj.org">SML/NJ</a> in hopes that their grammar and lexer would shed insight on what I was (obviously) doing wrong.  But, inasmuch as SL is like SML, the grammar and lexer were the same.

Sleep beckoned, so I went.  This morning I banged my head at it some more.  Then once I started combing over the documentation, it hit me.  ML-Yacc produces error-correcting parsers.  It will perform single-token substitutions in order to get a valid parse.  And, if you notice, it only has to make a single-token correction to get from the bad code to the good code.

My solution?  The same as SML/NJ's, set the lookahead to zero for interactive sessions and fail fast, so that if you are trying stuff interactively (or from unit tests) it will be relentless about grammar.  On the other hand, if you are parsing a file, my interpreter will be forgiving.  After all, why should it fail the whole file if all you're missing is <code>fn</code>?
