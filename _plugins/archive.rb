module Jekyll
  class Site
    attr_accessor :years

    def pages_by_year
      years = {}
      self.posts.each do |post|
        years[post.date.year] = [] if years[post.date.year].nil?
        years[post.date.year] << post
      end
      years
    end

    alias orig_site_payload site_payload
    def site_payload
      result = orig_site_payload
      result['site']['years'] = self.years
      result
    end
  end

  class ArchiveIndex < Page
    attr_reader :year
    def initialize(site, base, dir, year, posts)
      @site = site
      @base = base
      @dir = dir
      @name = 'index.html'
      @year = year
      self.process(@name)
      archive_index = (site.config['archive_index_layout'] || 'archive_index') + '.html'
      self.read_yaml(File.join(base, '_layouts'), archive_index)
      self.data['year'] = year
      self.data['posts'] = posts.sort {|a,b| b.date <=> a.date}
      self.data['title'] = year.to_s
    end

    def to_liquid
      result = super
      result['url'] = @dir + '/'
      result
    end
  end

  class ArchiveGenerator < Generator
    safe true

    def generate(site)
      return unless site.layouts.key? 'archive_index'
      years = site.pages_by_year
      site.years = []
      years.keys.sort {|a,b| b <=> a}.each do |year|
        index = ArchiveIndex.new(site, site.source, "/#{year}", year, years[year].to_a)
        site.pages << index
        site.years << index
      end
    end
  end
end
