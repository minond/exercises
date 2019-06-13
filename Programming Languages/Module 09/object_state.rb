class A
  ONE = 1

  @@global_increment = 0

  def initialize (f = 0)
    @local_increment = f
  end

  def local_increment
    @local_increment
  end

  def global_increment
    @@global_increment
  end

  def self.global_increment
    @@global_increment
  end

  def increment
    @local_increment += ONE
    @@global_increment += ONE
  end
end

a = A.new 1
b = A.new 10

a.increment
a.increment
a.increment
a.increment
a.increment
a.increment

puts "A.global_increment: #{A.global_increment}"

puts "a.local_increment: #{a.local_increment}"
puts "a.local_increment: #{a.global_increment}"

puts "b.local_increment: #{b.local_increment}"
puts "b.local_increment: #{b.global_increment}"
