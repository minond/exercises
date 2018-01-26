class C
  def m1
    print "hi "
    self
  end

  def m2
    print "bye "
    self
  end

  def m3
    print "\n"
    self
  end
end

c = C.new

c.m1
c.m1.m2
c.m1.m2.m1.m2
c.m1.m2.m1.m2.m3
