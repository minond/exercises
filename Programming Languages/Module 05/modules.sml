structure MyMathModule =
struct
  fun double x =
    x * 2
end

open MyMathModule

val a1 = double 10 (* We can do this because of the `open MyMathModule` above *)
val a2 = MyMathModule.double 10
