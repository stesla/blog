class Integer
  def ordinalize
    str = self.to_s
    str + case str
          when /11$/, /12$/, /13$/ then 'th'
          when /1$/ then 'st'
          when /2$/ then 'nd'
          when /3$/ then 'rd'
          else 'th'
          end
  end
end

module Jekyll
  module Filters
    def date_to_blog_string(date)
      date.strftime("%B #{date.day.ordinalize}, %Y")
    end
  end
end
