class A
  def initialize (f = 0)
    @foo = f
  end

  def foo
    @foo
  end

  def increment
    @foo += 1
  end
end

a = A.new 10
a.increment
a.increment
a.increment
puts a.foo
