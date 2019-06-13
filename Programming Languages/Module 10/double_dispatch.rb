class LExp
end

class LInt < LExp
  attr_reader :i

  def initialize i
    @i = i
  end

  def eval
    self
  end

  def add e
    e.addInt self
  end

  def addInt e
    LInt.new @i + e.i
  end

  def addRational e
    LRational.new @i + e.r
  end

  def addString e
    LString.new e.s + @i.to_s
  end
end

class LRational < LExp
  attr_reader :r

  def initialize r
    @r = r
  end

  def eval
    self
  end

  def add e
    e.addRational self
  end

  def addInt e
    LInt.new @r + e.i
  end

  def addRational e
    LRational.new @r + e.r
  end

  def addString e
    LString.new e.s + @r.to_s
  end
end

class LString < LExp
  attr_reader :s

  def initialize s
    @s = s
  end

  def eval
    self
  end

  def add e
    e.addString self
  end

  def addInt e
    LInt.new e.i.to_s + @s
  end

  def addRational e
    LRational.new e.r.to_s + @s
  end

  def addString e
    LString.new e.s + @s
  end
end

class LAdd < LExp
  attr_reader :e1, :e2

  def initialize(e1, e2)
    @e1 = e1
    @e2 = e2
  end

  def eval
    e1.eval.add e2.eval
  end
end


i1 = LInt.new 40
i2 = LInt.new 2

s1 = LString.new "hi "
s2 = LString.new "there"

add1 = LAdd.new i1, i2
add2 = LAdd.new s1, s2
add3 = LAdd.new s1, i1
add4 = LAdd.new i1, s2

puts "i1 = #{i1.inspect}"
puts "i2 = #{i2.inspect}"
puts "s1 = #{s1.inspect}"
puts "s2 = #{s2.inspect}"
puts "#{add1.inspect} = #{add1.eval}"
puts "#{add2.inspect} = #{add2.eval}"
puts "#{add3.inspect} = #{add3.eval}"
puts "#{add4.inspect} = #{add4.eval}"
