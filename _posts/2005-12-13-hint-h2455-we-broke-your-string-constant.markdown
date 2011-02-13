--- 
wordpress_id: 39
layout: post
title: "Delphi Hint H2455: We Broke Your String Constant"
wordpress_url: http://www.alieniloquent.com/?p=39
---
<p>So, Brian and I are going along writing code that was <em>supposed</em> to be <em>easy</em>.  Then we run into a problem.  Some of the super secure encryption we do to obfuscate some constants in our code wasn't working right &mdash; in .NET.  It worked fine in Win32.  So, we put on our spelunking hats and settled into our harnesses and went down into the depths of our code.</p>

<p>What did we find?  We found hint H2455.  It says "Narrowing given wide string constant lost information."  The helpful help says that if it encounters characters with ASCII values above 127 it <em>may</em> yes, I said may, replace them with a '?'.  How nice of them.</p>

<p>So we try declaring a typed constant as an AnsiString, but no, why would that work?  The string literal is still WideString.  We look at the assembly and sure enough, the byte 0x97 is getting replaced with 0x3F: '?'.</p>

<p>What did we end up doing?  We ditched the string constants.  We use an <code>array of Byte</code> and we made a function that will make an AnsiString out of it.  As a nice conclusion to this little vignette, that array is prettier to look at.</p>
