class A
  attr_accessor :x

  def initialize x
    @x = x
  end

  def x2v1
    # Directly gets an instance variable
    @x * @x
  end

  def x2v2
    # Gets an instance variable through the getter which does a dynamic
    # dispatch to a sub class, if any. This is the case because Ruby maps
    # `self` to the calling class.
    x * x
  end
end

class B < A
  def x
    7
  end
end


a = A.new 5
b = B.new 5

puts a.x2v1
puts a.x2v2

puts b.x2v1
puts b.x2v2
