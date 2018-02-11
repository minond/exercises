module Doubler
  def double
    self + self
  end
end

class Pt
  attr_accessor :x, :y
  include Doubler

  def + other
    ans = Pt.new
    ans.x = self.x + other.x
    ans.y = self.y + other.y
    ans
  end
end

class String
  include Doubler
end


pt = Pt.new
pt.x = 21
pt.y = 7
puts pt.double.inspect
puts "hi".double
