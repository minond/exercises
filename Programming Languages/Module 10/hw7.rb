# University of Washington, Programming Languages, Homework 7, hw7.rb
# (See also ML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

# 3. Complete the Ruby implementation except for intersection, which means skip
# for now additions to the Intersect class and, more importantly, methods
# related to intersection in other classes. Do not modify the code given to
# you. Follow this approach:
#
#  - Every subclass of GeometryExpression should have a preprocess_prog method
#    that takes no arguments and returns the geometry object that is the result
#    of preprocessing self. To avoid mutation, return a new instance of the
#    same class unless it is trivial to determine that self is already an
#    appropriate result.
#
#  - Every subclass of GeometryExpression should have an eval_prog method that
#    takes one argu- ment, the environment, which you should represent as an
#    array whose elements are two-element arrays: a Ruby string (the variable
#    name) in index 0 and an object that is a value in our language in index 1.
#    As in any interpreter, pass the appropriate environment when evaluating
#    subexpres- sions. (This is fairly easy since we do not have closures.) To
#    make sure you handle both scope and shadowing correctly:
#
#    - Do not ever mutate an environment; create a new environment as needed
#      instead. Be careful what methods you use on arrays to avoid mutation.
#
#    - The eval_prog method in Var is given to you. Make sure the environments
#      you create work correctly with this definition.
#
#    The result of eval_prog is the result of “evaluating the expression
#    represented by self,” so, as we expect with OOP style, the cases of ML’s
#    eval_prog are spread among our classes, just like with preprocess_prog.
#
#  - Every subclass of GeometryValue should have a shift method that takes two
#    arguments dx and dy and returns the result of shifting self by dx and dy.
#    In other words, all values in the language “know how to shift themselves
#    to create new objects.” Hence the eval_prog method in the Shift class
#    should be very short.
#
#  - Remember you should not use any method like is_a?, instance_of?, class,
#    etc.
#
#  - Analogous to SML, an overall programe would be evaluated via
#    e.preprocess_prog.eval_prog [] (notice we use an array for the
#    environment).

class GeometryExpression
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2)
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2)
    real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2)
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2)
  # the line containing seg, then we return the intersection of the
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end

  def shift x, y
    Point.new(@x + x, @y + y)
  end

  def eval_prog env
    self
  end

  def preprocess_prog
    self
  end

  def intersect b
    b.intersectPoint self
  end

  def intersectPoint p
    if real_close(@x, p.x) and real_close(@y, p.y)
      Point.new(@x, @y)
    else
      NoPoints.new
    end
  end

  def intersectLine line
    if real_close(@y, line.m * @x + line.b)
      Point.new(@x, @y)
    else
      NoPoints.new
    end
  end

  def intersectVerticalLine vline
    if real_close(@x, vline.x)
      Point.new(@x, @y)
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult seg
    x1 = [seg.x1, seg.x2].min - GeometryExpression::Epsilon
    x2 = [seg.x1, seg.x2].max + GeometryExpression::Epsilon
    y1 = [seg.y1, seg.y2].min - GeometryExpression::Epsilon
    y2 = [seg.y1, seg.y2].max + GeometryExpression::Epsilon

    if @x.between?(x1, x2) and @y.between?(y1, y2)
      Point.new(@x, @y)
    else
     NoPoints.new
    end
  end
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b

  def initialize(m,b)
    @m = m
    @b = b
  end

  def eval_prog env
    self
  end

  def shift x, y
    Line.new(@m, @b + y - @m * x)
  end

  def preprocess_prog
    self
  end

  def intersect b
    b.intersectLine self
  end

  def intersectPoint point
    point.intersectLine self
  end

  def intersectLine line
    if real_close(@m, line.m)
      if real_close(@b, line.b)
        Line.new(@m, @b)
      else
        NoPoints.new
      end
    else
      x = (line.b - @b) / (@m - line.m)
      y = @m * x + @b
      Point.new(x, y)
    end
  end

  def intersectVerticalLine vline
    Point.new(vline.x, @m * vline.x + @b)
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x

  def initialize x
    @x = x
  end

  def eval_prog env
    self
  end

  def shift x, y
    VerticalLine.new(@x + x)
  end

  def preprocess_prog
    self
  end

  def intersect b
    b.intersectVerticalLine self
  end

  def intersectPoint point
    point.intersectVerticalLine self
  end

  def intersectLine line
    line.intersectVerticalLine self
  end

  def intersectVerticalLine vline
    if real_close(@x, vline.x)
      VerticalLine.new(@x)
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def eval_prog env
    self
  end

  def shift x, y
    LineSegment.new(@x1 + x, @y1 + y, @x2 + x, @y2 + y)
  end

  # No LineSegment anywhere in the expression has endpoints that are the same
  # as (i.e., real close to) each other. Such a line-segment should be replaced
  # with the appropriate Point. For example in ML syntax,
  # LineSegment(3.2,4.1,3.2,4.1) should be replaced with Point(3.2,4.1).
  #
  # Every LineSegment has its first endpoint (the first two real values in SML)
  # to the left (lower x-value) of the second endpoint. If the x-coordinates of
  # the two endpoints are the same (real close), then the LineSegment has its
  # first endpoint below (lower y-value) the second endpoint. For any
  # LineSegment not meeting this requirement, replace it with a LineSegment
  # with the same endpoints reordered.
  def preprocess_prog
    if real_close(@x1, @x2) and real_close(@y1, @y2)
      Point.new(@x1, @y2)
    elsif y2 < y1
      LineSegment.new(@x2, @y2, @x1, @y1)
    else
      self
    end
  end

  def intersect b
    b.intersectLineSegment self
  end

  def intersectPoint point
    point.intersectLineSegment self
  end

  def intersectLine line
    line.intersectLineSegment self
  end

  def intersectVerticalLine vline
    self
  end

  def intersectWithSegmentAsLineResult seg2
    tx1 = [@x1, seg2.x1].max
    tx2 = [@x2, seg2.x2].min
    ty1 = [@y1, seg2.y1].max
    ty2 = [@y2, seg2.y2].min

    if tx1 > tx2 or ty1 > ty2
      NoPoints.new
    else
      LineSegment.new(tx1, ty1, tx2, ty2)
    end
  end
end

# Note: there is no need for getter methods for the non-value classes

# 4. Implement intersection in your Ruby solution following the directions
# here, in which we require both double dispatch and a separate use of dynamic
# dispatch for the line-segment case. Remember all the different cases in ML
# will appear somewhere in the Ruby solution, just arranged very differently.
#
#  - Implement preprocess_prog and eval_prog in the Intersect class. This is
#    not difficult, much like your prior work in the Shift class is not
#    difficult. This is because every subclass of GeometryValue will have an
#    intersect method that “knows how to intersect itself” with another
#    geometry-value passed as an argument.
#
#  - Every subclass of GeometryValue needs an intersect method, but these will
#    be short. The argument is another geometry-value, but we do not know what
#    kind. So we use double dispatch and call the appropriate method on the
#    argument passing self to the method. For example, the Point class has an
#    intersect method that calls intersectPoint with self.
#
#  - So methods intersectNoPoints, intersectPoint, intersectLine,
#    intersectVerticalLine, and intersectLineSegment defined in each of our 5
#    subclasses of GeometryValue handle the 25 possible intersection
#    combinations:
#
#      - The 9 cases involving NoPoints are done for you. See the GeometryValue
#        class — there is nothing more you need to do.
#
#      - Next do the 9 remaining cases involving combinations that do not
#        involve LineSegment. You will need to understand double-dispatch to
#        avoid is_a? and instance_of?. As in the ML code, 3 of these 9 cases
#        can just use one of the other cases because intersection is
#        commutative.
#
#      - What remains are the 7 cases where one value is a LineSegment and the
#        other is not NoPoints. These cases are all “done” for you because all
#        subclasses of GeometryValue in- herit an intersectLineSegment method
#        that will be correct for all of them. But it calls
#        intersectWithSegmentAsLineResult, which you need to implement for each
#        subclass of GeometryValue. Here is how this method should work:
#
#          * It takes one argument, which is a line segment. (In ML the
#            corresponding variable was a real*real*real*real, but here it will
#            actually be an instance of LineSegment and you can use the getter
#            methods x1, y1, x2, and y2 as needed.)
#
#          * It assumes that self is the intersection of (1) some not-provided
#            geometry-value and (2) the line (vertical or not) containing the
#            segment given as an argument.
#
#          * It returns the intersection of the not-provided geometry-value and
#            the segment given as an argument.
#
#        Together the 5 intersectWithSegmentAsLineResult methods you write will
#        implement the same algorithm as on lines 110–169 of the ML code.
class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end

  def eval_prog env
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def eval_prog env
    @e2.eval_prog((env + []).unshift([@s, @e1.eval_prog(env)]))
  end

  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end

  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end

  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def eval_prog env
    @e.eval_prog(env).shift(@dx, @dy)
  end

  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end
end
