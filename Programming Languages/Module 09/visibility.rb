class Foo
  def initialize
    @bar = "Bar"
  end

  def bar
    @bar
  end

  def bar= s
    @bar = s
  end
end

foo = Foo.new
puts foo.bar
foo.bar = "Quux"
puts foo.bar



class Foobar
  attr_accessor :bar

  def initialize
    @bar = "Bar"
  end
end

foobar = Foobar.new
puts foobar.bar
foobar.bar = "Quux"
puts foobar.bar



class Bar
  def implicit_public_method
    0
  end

  public
  def explicit_public_method
    1
  end

  protected
  def protected_method
    2
  end

  private
  def private_method
    3
  end
end

bar = Bar.new
puts bar.implicit_public_method
puts bar.explicit_public_method
