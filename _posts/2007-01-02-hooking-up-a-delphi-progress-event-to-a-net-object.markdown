--- 
wordpress_id: 97
layout: post
title: Hooking up a Delphi progress event to a .NET object
wordpress_url: http://blog.alieniloquent.com/2007/01/02/hooking-up-a-delphi-progress-event-to-a-net-object/
---
So we know how to create a DLL in C++ that exposes .NET code to the Win32 world.  We also know how to consume that DLL from Delphi.  We know how to instantiate an object, call methods on it, and destroy it.  So now, let's do something interesting.  Let's make a progress bar.

Since this is just an example, we're going to do something really simple.  We'll make an object that runs for a number of cycles and calls our event handler each cycle after sleeping for a little bit.  Here's the code for <code>Clock</code>:

<pre class="code">
// Example.h
public ref class Clock
{
public:
  Clock(): 
    _progressCallback(gcnew ProgressCallback(NULL, NULL)) {}
  ~Clock() {}
  void SetProgressCallback(ProgressCallback ^ callback)
  {
    _progressCallback = callback;
  }
  void Run(int cycles);
private:
  ProgressCallback ^ _progressCallback;
};
<br />
// Example.cpp
void Example::Clock::Run(int cycles)
{
  for (int i = 1; i &lt;= cycles; ++i)
  {
    Thread::Sleep(250); // A noticeable pause
    _progressCallback->Execute(i, cycles);
  }
}
</pre>

Now the first thing to notice there is the <code>ref</code> keyword.  This is how you let the compiler know this is a managed class.  Next, you're all probably wondering what <code>ProgressCallback</code> is.  That is the class that takes care of all the magic behind simulating method pointers from Delphi.

A brief aside to talk about just what method pointers are.  In C and C++ you can declare a pointer type like this:

<pre class="code">
typedef int (* CALLBACK)(int x, int y);
</pre>

Then you can use that type like this:

<pre class="code">
int Apply(CALLBACK cb, int x, int y)
{
  return cb == NULL ? 0 : cb(x, y);
}
<br />
int Multiply(int x, int y)
{
  return x * y;
}
<br />
Apply(Multiply, 6, 7); // returns 42
</pre>

You can do the exact same thing in Delphi like this:

<pre class="code">
type TCallback = function(X, Y: Integer): Integer;
</pre>

But Delphi also offers another kind of function pointer called a method pointer.  You declare it like this:

<pre class="code">
type TMethodCallback = function(X, Y: Integer): Integer of object;
</pre>

Those two words <code>of object</code> make all the difference.  What this does is it lets you use a pointer to a method on a specific instance of an object.  When you call that method the <code>Self</code> pointer is set to the correct value so that you can access the state on the object.  It is really powerful.  This is typically how progress bars are driven in VCL applications.  You just do something like this:

<pre class="code">
type TProgressEvent = procedure(ACurrent, AMax: Integer) of object;
<br />
// ...
<br />
type TMyForm = class(TForm)
// ... stuff ...
  Progress(ACurrent, AMax: Integer);
// ... more stuff ...
end;
<br />
// ... then somewhere in the implementation ...
procedure TForm1.InitializeStuff;
begin
//  ... Initialize some things ...
  FThingWithProgressEvent.OnProgress := Progress;
// ... Initialize more things ...
end;
</pre>

And then that method can do something such as adjust a progress bar or log to a file.  It's really slick.

Well, we want to display a progress bar in our Delphi GUI that moves as our <code>Clock</code> ticks across.  But we can only pass plain old procedure pointers (the kind without <code>of object</code>) to the DLL functions because the code in the DLL doesn't know how to do the magic that makes method pointers so nice.  So we'll just have to make that magic happen ourselves by passing the object pointer in explicitly along with a procedure pointer that takes the object pointer in its parameter list.  We can cast the pointer and then call a method on the object with the rest of the parameters.

So now that we have the basic strategy in mind.  Let me show you the code that encapsulates this method pointer idea:

<pre class="code">
// Example.h
public ref class ProcedureOfObject
{
public:
  ProcedureOfObject(void * object, void * procedure):
    _object((IntPtr) object), _procedure((IntPtr) procedure) {}
protected:
  property bool HasNullPointers
  {
    bool get() 
    {
      return ObjectPointer == NULL || 
        ProcedurePointer == NULL; 
    }
  }
  property void * ObjectPointer
  {
    void * get() { return _object->ToPointer(); }
  }
  property void * ProcedurePointer
  {
    void * get() { return _procedure->ToPointer(); }
  }
private:
  IntPtr^ _object;
  IntPtr^ _procedure;
};
<br />
typedef void (* PROGRESSEVENT)(void *, int, int);
public ref class ProgressCallback : public ProcedureOfObject
{
public:
  ProgressCallback(void * object, void * procedure): ProcedureOfObject(object, procedure) {}	
  void Execute(int current, int max);
};
<br />
// Example.cpp
void Example::ProgressCallback::Execute(int current, int max)
{
  if (this->HasNullPointers)
    return;
  ((Example::PROGRESSEVENT) ProcedurePointer)(this->ObjectPointer, current, max);
}
</pre>

Note that I store the pointers as <code>IntPtr</code> references.  This is the type that all of the methods on <code>System::Runtime::InteropServices::Marshal</code> return pointers as.  So, it's useful to make your fields that way.  You can always call <code>ToPointer()</code> on it.

Now, the last bit that we need is to export stuff in the DLL.  But you'll notice that all of the classes I've made so far have been managed classes.  We can't send a pointer to a managed object out of the DLL, but we can send a pointer to an unmanaged object that has a reference to our managed object.  So we make this wrapper:

<pre class="code">
// Example.h
public class ClockWrapper
{
public:
  ClockWrapper(): _clock(gcnew Clock()) {}
  ~ClockWrapper() {}
  void SetProgressCallback(void * object, PROGRESSEVENT callback)
  {
    _clock->SetProgressCallback(gcnew ProgressCallback(object, callback));
  }
  void Run(int cycles)
  {
    _clock->Run(cycles);
  }
private:
  gcroot&lt;Clock ^&gt; _clock;
};
</pre>

So all that's left is to export the DLL functions like before.  Just to keep them separate I'll make another delete method, even though it's identical in every way except the name.

<pre class="code">
// Exports.h
DLLAPI void * ClockCreate();
DLLAPI void ClockDelete(void * clock);
DLLAPI void ClockRun(void * clock, int cycles);
DLLAPI void ClockSetProgressCallback(void * clock, void * object, PROGRESSEVENT callback);
<br />
// Exports.cpp
#define C(p) ((ClockWrapper *) p)
<br />
DLLAPI void * ClockCreate()
{
  return new ClockWrapper();
}
<br />
DLLAPI void ClockDelete(void * clock)
{
  delete clock;
}
<br />
DLLAPI void ClockRun(void * clock, int cycles)
{
  C(clock)->Run(cycles);
}
<br />
DLLAPI void ClockSetProgressCallback(void * clock, void * object, PROGRESSEVENT callback)
{
  C(clock)->SetProgressCallback(object, callback);
}
</pre>

Then on the Delphi side:

<pre class="code">
// interface
type TForm1 = class(TForm)
  edtCycles: TEdit;
  btnCycle: TButton;
  ProgressBar1: TProgressBar;
  procedure btnCycleClick(Sender: TObject);
private
  procedure Progress(ACurrent, AMax: Integer);
end;
<br />
// implementation
type 
  TProgressEvent = procedure(AObject: Pointer; ACurrent, AMax: Integer); cdecl;
  PForm1 = ^TForm1;
<br />
function ClockCreate: Pointer;
  cdecl; external 'Example';
procedure ClockDelete(AClock: Pointer);
  cdecl; external 'Example';
procedure ClockRun(AClock: Pointer; ACycles: Integer);
  cdecl; external 'Example';
procedure ClockSetProgressCallback(AClock: Pointer; AObject: Pointer; ACallback: TProgressEvent); 
  cdecl; external 'Example';
<br />
procedure ProgressCallback(AObject: Pointer; ACurrent, AMax: Integer); cdecl;
begin
  PForm1(AObject).Progress(ACurrent, AMax);
  Application.ProcessMessages;
end;
<br />
procedure TForm1.btnCycleClick(Sender: TObject);
var
  Clock: Pointer;
begin
  Clock := ClockCreate();
  try
    ProgressBar1.Position := 0;
    ClockSetProgressCallback(Clock, @Self, ProgressCallback);
    ClockRun(Clock, StrToInt(edtCycles.Text));
  finally
    ClockDelete(Clock);
  end;
end;
<br />
procedure TForm1.Progress(ACurrent, AMax: Integer);
begin
  ProgressBar1.Max := AMax;
  ProgressBar1.Position := ACurrent;
end;
</pre>

Some things to note.  First off, the <code>Progress</code> method on <code>TForm1</code> is <code>private</code>, and yet I call it from <code>ProgressCallback</code>.  This is because of how scoping works in Delphi.  Any code in the same unit as a <code>private</code> or <code>protected</code> method can call that method.  In Delphi 2005, the keywords <code>strict private</code> and <code>strict protected</code> were introduced to prevent this ability, but in this case we actually <em>want</em> the behavior because we don't want to expose that event to anybody else.

Next, notice that the procedure pointer also has <code>cdecl</code> on it.  This has to be this way because it's a pointer that will be passed into the DLL.  So, it needs to be declared with the same calliing convention that'll be used on the other side.

That's how you hook up a Delphi progress bar to a .NET object.  It isn't much more work to use the <code>ProcedureOfObject</code> pattern and call that from a delegate on the .NET side, which is useful when you are hooking events on sub-objects.

One big gotcha with this pattern that isn't demonstrated in this code is hooking the callback in a constructor.  The <code>Self</code> pointer does not point to what you think it does.  So if you send that along to the other side and call back to it later, you're referencing offsets off of something else entirely, and you will get access violations.

So now we know how to make a .NET DLL that we can call into from the Win32 world.  We know how to consume that in Delphi, and we know how to simulate Delphi's method pointers.  With these building blocks, you can map just about anything in the .NET world into your Delphi applications.
