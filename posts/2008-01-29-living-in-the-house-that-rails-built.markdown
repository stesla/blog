---
title: Living In the House That Rails Built
---
I wanted to share a snippet of code. This code will print a call stack to
`STDOUT` every time a Ruby class definition is evaluated. It is particularly
useful when you find that class constants are being mysteriously redefined.

~~~~ {.code}
class Foo
  puts "\nRequired from:\n #{Kernel.caller.join("\n ")}"
  # ...
end
~~~~

What inspired me to write that code? Rails did. The key to writing Ruby on
Rails is that you're writing Ruby _on Rails_. You don't follow the Rails best
practices because they're convenient. You follow the Rails best practices
because your program won't work unless you do. Just like trains, you stay on
the track and everything is great. If you try to take your train off-track,
then it's gruesome enough to make the nightly news.

How did I derail my application such that I cared how and where a file was
being required? I wrote a unit test that explicitly required a model object.
Oops. Remember that the semantics of `require` is load-once based on the name.
So:

~~~~ {.code}
require "foo"
~~~~

and:

~~~~ {.code}
require "models/foo"
~~~~

are very different to require. Rails is super helpful and requires everything
that it makes for you. So it requires models for you, even when you run your
unit tests.

So take this code:

~~~~ {.code}
class Foo < ActiveRecord::Base
  RAILS_IS_A_GHETTO = true
end
~~~~

And then write a test for something that Rails didn't generate (such as
something in the `lib` directory like I did):

~~~~ {.code}
# Require some other stuff
require "foo"

class TestTruth < Test::Unit::TestCase
  def test_truth
    assert true
  end
end
~~~~

If you `rake test` you will get an error complaining that `RAILS_IS_A_GHETTO`
was reinitialized, and that's because Rails loads it for you as "models/foo"
and you load it as "foo" so it gets loaded twice.

The moral of the story is: let Rails load the things it built, and you load
the things you built.
