class Point
  attr_accessor :x, :y
end

class ColorPoint < Point
end

p = Point.new
c = ColorPoint.new

puts "c.instance_of? ColorPoint = #{c.instance_of? ColorPoint}"
puts "c.instance_of? Point = #{c.instance_of? Point}"
puts "c.is_a? ColorPoint = #{c.is_a? ColorPoint}"
puts "c.is_a? Point = #{c.is_a? Point}"

puts "p.instance_of? ColorPoint = #{p.instance_of? ColorPoint}"
puts "p.instance_of? Point = #{p.instance_of? Point}"
puts "p.is_a? ColorPoint = #{p.is_a? ColorPoint}"
puts "p.is_a? Point = #{p.is_a? Point}"
