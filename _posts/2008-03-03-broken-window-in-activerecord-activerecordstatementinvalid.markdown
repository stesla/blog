--- 
layout: post
wordpress_id: 126
title: "Broken Window in ActiveRecord: <code>ActiveRecord::StatementInvalid</code>"
wordpress_url: http://blog.alieniloquent.com/2008/03/03/broken-window-in-activerecord-activerecordstatementinvalid/
--- |+
I love ruby, and I love Rails, but in some ways it really is a ghetto. It has
a lot of [broken windows][1] that only serve to encourage bad coding from
developers who should know better. Today I ran into an example of one of those
broken windows and I was beside myself. I could not believe what I was
reading.

One of the projects I work on for my employer is an import process that takes
a long time. In order to make it resilient to database fail-overs, I wanted to
catch the exception that is raised when the connection dies, wait a few
seconds, and then try to reconnect. The idea is simple, and it works once I
account for the broken window, but I am not pleased with the code I had to
write.

When the database connection disappears, the database driver throws an
exception. `ActiveRecord::Base` catches that exception and does this:

{% highlight text %}# Find this in Rails 2.0.2

# active_record/connection_adapter/abstract_adapter.rb:121

rescue Exception => e

# Log message and raise exception.

# Set last_verfication to 0, so that connection gets verified

# upon reentering the request loop

@last_verification = 0

message = "#{e.class.name}: #{e.message}: #{sql}"

log_info(message, name, 0)

raise ActiveRecord::StatementInvalid, message

end{% endhighlight %}

This is the exception handler that catches all exceptions raised during a
query run by ActiveRecord. As you can see, it snags the class name, and the
exception message off of the exception, and then throws the object away,
reraising with `ActiveRecord::StatementInvalid`. So, if your database driver
has hundreds of error codes which are provided in order for you to tell
specifically what error occurred, such as `Mysql::Error`, you lost them.

So ActiveRecord provides _one_ exception that covers everything from primary
key violations to database connection errors, and the only way to distinguish
them is by inspecting the message. Surely, that can't be true, right? I dig
further and find this:

{% highlight text %}# Find this in Rails 2.0.2

# active_record/connection_adapters/mysql_adapter.rb:244

#

# Note: I snipped the error message because it is very long

rescue ActiveRecord::StatementInvalid => exception

if exception.message.split(":").first =~ /Packets out of order/

raise ActiveRecord::StatementInvalid, snipped_error_message

else

raise

end

end

{% endhighlight %}

That is just completely unacceptable. I can find it in my heart to forgive the
abstract adapter for doing something that throws away implementation-specific
information, but the Mysql adapter should remedy that. It willingly lets it's
exception information be cast aside and goes about inspecting what the
abstract adapter had the decency to keep around.

"But that information is good enough to tell what the exception is," you might
say.

Until the Mysql folks change the error message. The Mysql API exposes numeric
constants, and I'm sure they're very careful to keep them the same, but do you
think they take the same approach to error messages? I doubt it. They provide
a function that will give you an error message given the numeric constant, and
encourage you to use it. That's what the Mysql bindings for ruby do.

Expecting developers to inspect the exception message is essentially promoting
programming with magic numbers. Sure, they're string literals, but they're
still duplicated information, and extremely brittle.

All I'd want is an `inner_exception` attribute available on
`ActiveRecord::StatementInvalid` or maybe its parent, and then assign it when
doing reraises. Is that too much to ask for?

   [1]: http://en.wikipedia.org/wiki/Fixing_Broken_Windows

