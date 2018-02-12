(* University of Washington, Programming Languages, Homework 7, hw7.sml
   (See also Ruby code.)
*)

(* Do not make changes to this code except where you see comments containing
   the word CHANGE. *)

(* expressions in a little language for 2D geometry objects
   values: points, lines, vertical lines, line segments
   other expressions: intersection of two expressions, lets, variables,
                      (shifts added by you)
*)
datatype geom_exp =
           NoPoints
	 | Point of real * real (* represents point (x,y) *)
	 | Line of real * real (* represents line (slope, intercept) *)
	 | VerticalLine of real (* x value *)
	 | LineSegment of real * real * real * real (* x1,y1 to x2,y2 *)
	 | Intersect of geom_exp * geom_exp (* intersection expression *)
	 | Let of string * geom_exp * geom_exp (* let s = e1 in e2 *)
	 | Var of string
   (* CHANGE add shifts for expressions of the form Shift(deltaX, deltaY, exp *)

   (* 2. Add shift expressions as defined above to the SML implementation by
    * adding the constructor Shift of real * real * geom_exp to the definition
    * of geom_exp and adding appropriate branches to eval_prog and
    * preprocess_prog. (The first real is deltaX and the second is deltaY .)
    * Do not change other functions. In particular, there is no need to change
    * intersect because this function is used only for values in our geometry
    * language and shift expressions are not geometry values. *)
   | Shift of real * real * geom_exp

exception BadProgram of string
exception Impossible of string

(* helper functions for comparing real numbers since rounding means
   we should never compare for equality *)

val epsilon = 0.00001

fun real_close (r1,r2) =
    (Real.abs (r1 - r2)) < epsilon

(* notice curried *)
fun real_close_point (x1,y1) (x2,y2) =
    real_close(x1,x2) andalso real_close(y1,y2)

(* helper function to return the Line or VerticalLine containing
   points (x1,y1) and (x2,y2). Actually used only when intersecting
   line segments, but might be generally useful *)
fun two_points_to_line (x1,y1,x2,y2) =
    if real_close(x1,x2)
    then VerticalLine x1
    else
	let
	    val m = (y2 - y1) / (x2 - x1)
	    val b = y1 - m * x1
	in
	    Line(m,b)
	end

(* helper function for interpreter: return value that is the intersection
   of the arguments: 25 cases because there are 5 kinds of values, but
   many cases can be combined, especially because intersection is commutative.
   Do *not* call this function with non-values (e.g., shifts or lets)
 *)
fun intersect (v1,v2) =
    case (v1,v2) of

       (NoPoints, _) => NoPoints (* 5 cases *)
     | (_, NoPoints) => NoPoints (* 4 additional cases *)

     | 	(Point p1, Point p2) => if real_close_point p1 p2
				then v1
				else NoPoints

      | (Point (x,y), Line (m,b)) => if real_close(y, m * x + b)
				     then v1
				     else NoPoints

      | (Point (x1,_), VerticalLine x2) => if real_close(x1,x2)
					   then v1
					   else NoPoints

      | (Point _, LineSegment seg) => intersect(v2,v1)

      | (Line _, Point _) => intersect(v2,v1)

      | (Line (m1,b1), Line (m2,b2)) =>
	if real_close(m1,m2)
	then (if real_close(b1,b2)
	      then v1 (* same line *)
	      else  NoPoints) (* parallel lines do not intersect *)
	else
	    let (* one-point intersection *)
		val x = (b2 - b1) / (m1 - m2)
		val y = m1 * x + b1
	    in
		Point (x,y)
	    end

      | (Line (m1,b1), VerticalLine x2) => Point(x2, m1 * x2 + b1)

      | (Line _, LineSegment _) => intersect(v2,v1)

      | (VerticalLine _, Point _) => intersect(v2,v1)
      | (VerticalLine _, Line _)  => intersect(v2,v1)

      | (VerticalLine x1, VerticalLine x2) =>
	if real_close(x1,x2)
	then v1 (* same line *)
	else NoPoints (* parallel *)

      | (VerticalLine _, LineSegment seg) => intersect(v2,v1)

      | (LineSegment seg, _) =>
	(* the hard case, actually 4 cases because v2 could be a point,
	   line, vertical line, or line segment *)
	(* First compute the intersection of (1) the line containing the segment
           and (2) v2. Then use that result to compute what we need. *)
	(case intersect(two_points_to_line seg, v2) of
	    NoPoints => NoPoints
	  | Point(x0,y0) => (* see if the point is within the segment bounds *)
	    (* assumes v1 was properly preprocessed *)
	    let
		fun inbetween(v,end1,end2) =
		    (end1 - epsilon <= v andalso v <= end2 + epsilon)
		    orelse (end2 - epsilon <= v andalso v <= end1 + epsilon)
		val (x1,y1,x2,y2) = seg
	    in
		if inbetween(x0,x1,x2) andalso inbetween(y0,y1,y2)
		then Point(x0,y0)
		else NoPoints
	    end
	  | Line _ => v1 (* so segment seg is on line v2 *)
	  | VerticalLine _ => v1 (* so segment seg is on vertical-line v2 *)
	  | LineSegment seg2 =>
	    (* the hard case in the hard case: seg and seg2 are on the same
               line (or vertical line), but they could be (1) disjoint or
               (2) overlapping or (3) one inside the other or (4) just touching.
	       And we treat vertical segments differently, so there are 4*2 cases.
	     *)
	    let
		val (x1start,y1start,x1end,y1end) = seg
		val (x2start,y2start,x2end,y2end) = seg2
	    in
		if real_close(x1start,x1end)
		then (* the segments are on a vertical line *)
		    (* let segment a start at or below start of segment b *)
		    let
			val ((aXstart,aYstart,aXend,aYend),
			     (bXstart,bYstart,bXend,bYend)) = if y1start < y2start
							      then (seg,seg2)
							      else (seg2,seg)
		    in
			if real_close(aYend,bYstart)
			then Point (aXend,aYend) (* just touching *)
			else if aYend < bYstart
			then NoPoints (* disjoint *)
			else if aYend > bYend
			then LineSegment(bXstart,bYstart,bXend,bYend) (* b inside a *)
			else LineSegment(bXstart,bYstart,aXend,aYend) (* overlapping *)
		    end
		else (* the segments are on a (non-vertical) line *)
		    (* let segment a start at or to the left of start of segment b *)
		    let
			val ((aXstart,aYstart,aXend,aYend),
			     (bXstart,bYstart,bXend,bYend)) = if x1start < x2start
							      then (seg,seg2)
							      else (seg2,seg)
		    in
			if real_close(aXend,bXstart)
			then Point (aXend,aYend) (* just touching *)
			else if aXend < bXstart
			then NoPoints (* disjoint *)
			else if aXend > bXend
			then LineSegment(bXstart,bYstart,bXend,bYend) (* b inside a *)
			else LineSegment(bXstart,bYstart,aXend,aYend) (* overlapping *)
		    end
	    end
	  | _ => raise Impossible "bad result from intersecting with a line")
      | _ => raise Impossible "bad call to intersect: only for shape values"

(* interpreter for our language:
   * takes a geometry expression and returns a geometry value
   * for simplicity we have the top-level function take an environment,
     (which should be [] for the whole program
   * we assume the expression e has already been "preprocessed" as described
     in the homework assignment:
         * line segments are not actually points (endpoints not real close)
         * lines segment have left (or, if vertical, bottom) coordinate first
*)

fun eval_prog (e,env) =
    case e of
	NoPoints => e (* first 5 cases are all values, so no computation *)
      | Point _  => e
      | Line _   => e
      | VerticalLine _ => e
      | LineSegment _  => e
      | Var s =>
	(case List.find (fn (s2,v) => s=s2) env of
	     NONE => raise BadProgram("var not found: " ^ s)
	   | SOME (_,v) => v)
      | Let(s,e1,e2) => eval_prog (e2, ((s, eval_prog(e1,env)) :: env))
      | Intersect(e1,e2) => intersect(eval_prog(e1,env), eval_prog(e2, env))
      (* CHANGE: Add a case for Shift expressions *)

      (* A Shift expression is not a value. It has a deltaX (a floating-point
       * number), a deltaY (a floating-point number), and a subexpression.
       * The semantics is to evaluate the subexpression and then shift the
       * result by deltaX (in the x-direction; positive is “to the right”) and
       * deltaY (in the y-direction; positive is “up”). More specifically,
       * shifting for each form of value is as follows:
       *
       *   - NoPoints remains NoPoints.
       *
       *   - A Point representing (x, y) becomes a Point representing (x +
       *     deltaX , y + deltaY ).
       *
       *   - A Line with slope m and intercept b becomes a Line with slope m
       *     and an intercept of b+deltaY −m·deltaX.
       *
       *   - A VerticalLine becomes a VerticalLine shifted by deltaX ; the
       *     deltaY is irrelevant.
       *
       *   - A LineSegment has its endpoints shift by deltaX and deltaY. *)
      | Shift(dx, dy, sube) =>
          case eval_prog (sube, env)
           of NoPoints => NoPoints
            | Point (x, y) => Point (x + dx, y + dy)
            | Line (slope, intercept) => Line (slope, intercept + dy - slope * dx)
            | VerticalLine (x) => VerticalLine (x + dx)
            | LineSegment (x1, y1, x2, y2) => LineSegment (x1 + dx, y1 + dy, x2 + dx, y2 + dy)
            | _ => raise BadProgram ("Shift applied to non-point or non-line value.")

(* CHANGE: Add function preprocess_prog of type geom_exp -> geom_exp *)

(* 1. Implement an SML function preprocess_prog of type geom_exp -> geom_exp
 * to implement expression preprocessing as defined above. The idea is that
 * evaluating program e would be done with eval_prog (preprocess_prog e, [])
 * where the [] is the empty list for the empty environment.
 *
 * To simplify the interpreter, we first preprocess expressions. Preprocessing
 * takes an expression and produces a new, equivalent expression with the
 * following invariants:
 *
 *   - No LineSegment anywhere in the expression has endpoints that are the same
 *     as (i.e., real close to) each other. Such a line-segment should be
 *     replaced with the appropriate Point. For example in ML syntax,
 *     LineSegment(3.2,4.1,3.2,4.1) should be replaced with Point(3.2,4.1).
 *
 *   - Every LineSegment has its first endpoint (the first two real values in
 *     SML) to the left (lower x-value) of the second endpoint. If the
 *     x-coordinates of the two endpoints are the same (real close), then the
 *     LineSegment has its first endpoint below (lower y-value) the second
 *     endpoint. For any LineSegment not meeting this requirement, replace it
 *     with a LineSegment with the same endpoints reordered. *)
fun preprocess_prog gx =
  case gx
   of LineSegment (x1, y1, x2, y2) =>
        (* Same endpoints? Or do we need to flip the values? *)
        (case (real_close (x1, x2), real_close (y1, y2), y2 < y1)
         of (true, true, _) => Point (x1, y1)
          | (true, _, true) => LineSegment (x2, y2, x1, y1)
          | _ => gx)
    | _ => gx
