class Point
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def distFromOrigin
    Math.sqrt(@x * @x + @y * @y)
  end

  def distFromOrigin2
    Math.sqrt(x * x + y * y)
  end
end

class ThreeDPoint < Point
  attr_accessor :z

  def initialize(x, y, z)
    super(x, y)
    @z = z
  end

  def distFromOrigin
    d = super
    Math.sqrt(d * d + @z * @z)
  end

  def distFromOrigin2
    d = super
    Math.sqrt(d * d + z * z)
  end
end

class PolarPoint < Point
  def initialize(r, theta)
    @r = r
    @theta = theta
  end

  def x
    @r * Math.cos(@theta)
  end

  def y
    @r * Math.sin(@theta)
  end

  def x= a
    b = y
    @theta = Math.atan(b / a)
    @r = Math.sqrt(a * a + b * b)
    self
  end

  def y= b
    a = y
    @theta = Math.atan(b / a)
    @r = Math.sqrt(a * a + b * b)
    self
  end
end
