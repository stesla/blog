--- 
wordpress_id: 96
layout: post
title: Calling into the C++ DLL from Delphi
wordpress_url: http://blog.alieniloquent.com/2007/01/01/calling-into-the-c-dll-from-delphi/
---
Before I get to the meat of this post, I want to make some ammendments and edits to the code from the last one.  Today I was wrangling around and began to recall more of my C++, initializers in particular, so I've updated the <code>Example1</code> class to use them.

<pre class="code">
public class Example1
{
public:
  Example1(const char * name) : _name(gcnew String(name)) {}
  ~Example1() {}
  void ShowName();
private:
  gcroot&lt;String ^&gt; _name;
};
</pre>

I also realize that I forgot to show the implementation side of that class, so here it is:

<pre class="code">
// Example.cpp
#include "stdafx.h"
#include "Example.h"
<br />
using namespace System::Windows::Forms;
<br />
void Example::Example1::ShowName()
{
  MessageBox::Show(_name);
}
</pre>

So there's our DLL.  Now, let's use it from Delphi!  I'm using Turbo Delphi for Win32 to do this.  Go to File &gt; New &gt; "VCL Forms Application" and make your project.  Make sure that it outputs to the same directory that the DLL does (or make the DLL output to the same directory this project does, which is what I do) for ease of edit-compile-run cycling.

I'm going to drop a <code>TEdit</code> and a <code>TButton</code> on the main form and hook it up so that when we click the button it creates an <code>Example1</code>, shows it, and then deletes it.  Here is the implementation section from the main unit in the delphi program:

<pre class="code">
function Example1Create(AName: PChar): Pointer; 
  cdecl; external 'Example';
procedure Example1Delete(AExample: Pointer);
  cdecl; external 'Example';
procedure Example1ShowName(AExample: Pointer);
  cdecl; external 'Example';
<br />
procedure TForm1.btnDoItClick(Sender: TObject);
var
  Example: Pointer;
begin
  Example := Example1Create(PChar(edtName.Text));
  try
    Example1ShowName(Example);
  finally
    Example1Delete(Example);
  end;
end;
</pre>

The button handler is straight-forward and normal.  The only interesting thing there is to see how the calls from the DLL get used.  The lifetime management works just like anything else, you just aren't going to use <code>FreeAndNil</code> like you would for most things.

The interesting part is at the top where we import from the DLL, let's look at one of those lines again:

<pre class="code">
function Example1Create(AName: PChar): Pointer; 
  cdecl; external 'Example';
</pre>

Now this corresponds to the following line from <code>Exports.h</code>:

<pre class="code">
DLLAPI void * Example1Create(const char * name);
</pre>

Since it has a return type that is not void, it becomes a function (the others became procedures).  The <code>void *</code> becomes <code>Pointer</code>, and the <code>const char * name</code> becomes <code>PChar</code> in Delphi.  So what's the rest of that garbage?  The <code>cdecl</code> flag is there to tell the compiler what calling convention to use.  If you just create the DLL in Visual Studio, it defaults to using <code>cdecl</code>.  You can also use something like <code>stdcall</code>, but it's not necessary here.  The other part just tells Delphi which DLL to look for this external function in.

So now we know how to call code from the DLL.  Next time I'll show you how to pass procedure pointers and even method pointers from Delphi into the DLL and have them get called properly for things like progress bars.
