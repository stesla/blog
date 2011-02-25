# This module is here to prevent posts migrated from WordPress from
# showing up in my RSS feed with different GUIDs and thus showing up
# as new posts again. None of them are remotely new, so I don't want
# to tease. Once I have more than 10 new posts, this can go away.

module Jekyll
  class RssHack < Generator
    def generate(site)
      site.config['rss_posts'] = site.posts.select do |post|
        post.date >= Time.utc(2011,2,25)
      end
    end
  end
end
