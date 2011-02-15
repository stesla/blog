--- 
layout: post
wordpress_id: 92
title: Making Active Directory liveable for Linux
wordpress_url: http://blog.alieniloquent.com/2006/09/14/making-active-directory-liveable-for-linux/
--- |+
So my employer's network is a Windows network. This isn't terribly unusual.
Our department's version control server, however, is a Linux box. We didn't
want to have to have more than one password, so back when I set it up, I took
the time to figure out how to get it to authenticate to Active Directory for
us. It's really cool, actually. [Samba][1] is really awesome.

However, we have had a few problems. The thing is that every so often, our
Linux server was being removed from the Windows domain. So then we couldn't
authenticate users, and so we couldn't check out our source code. Obviously,
that causes problems.

It turns out that adding one line to the `[global]` section of my `smb.conf`
file is what is needed to solve the problem. See, the Windows boxes check in
with the Active Directory controller once a day to update their machine ID. My
server was not doing this. I've found, over time, that there are a lot of
options for making Samba play well in a Windows network that aren't really
defaulted like they should be. Below is what I added to make it update like
the others.

{% highlight text %}machine password timeout = 86400 # one day{% endhighlight
%}

Another interesting thing, was that despite my not telling it to, Samba was
advertising the machine as a domain controller. So I had to add the line below
to disable that (despite the docs saying I shouldn't have to).

{% highlight text %}domain master = no{% endhighlight %}

In addition to the rest of the configuration for Samba to work with Active
Directory, and I think we may finally have it working all the way.

   [1]: http://samba.org

