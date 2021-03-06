---
title: Remember ML-Yacc makes error-correcting parsers
---
So I got my language [working][1]. But there are still some things I want to
add to it. One thing that was bothering me was that both this code:

~~~~ {.code}
fn x => x
~~~~

and this code:

~~~~ {.code}
x => x
~~~~

parsed to the _same thing_.

I banged my head against this. My grammar had the production right:

~~~~ {.code}
expr : ...
     | FN ident RARROW expr (T.FnDef (ident,expr))
     ...
~~~~

and my lexer produced the tokens just fine:

~~~~ {.code}
<INITIAL> "fn" => (Tokens.FN(!pos, !pos));
<INITIAL> "=>" => (Tokens.RARROW(!pos, !pos));
~~~~

So, I was confused. I downloaded the source for [SML/NJ][2] in hopes that
their grammar and lexer would shed insight on what I was (obviously) doing
wrong. But, inasmuch as SL is like SML, the grammar and lexer were the same.

Sleep beckoned, so I went. This morning I banged my head at it some more. Then
once I started combing over the documentation, it hit me. ML-Yacc produces
error-correcting parsers. It will perform single-token substitutions in order
to get a valid parse. And, if you notice, it only has to make a single-token
correction to get from the bad code to the good code.

My solution? The same as SML/NJ's, set the lookahead to zero for interactive
sessions and fail fast, so that if you are trying stuff interactively (or from
unit tests) it will be relentless about grammar. On the other hand, if you are
parsing a file, my interpreter will be forgiving. After all, why should it
fail the whole file if all you're missing is `fn`?

   [1]: /2007/10/08/samuels-lambda-and-the-y-combinator.html

   [2]: http://smlnj.org

