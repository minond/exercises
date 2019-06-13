xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]

p "xs.size should be 15 = #{xs.size}"
p "xs[0] should be 0 = #{xs[0]}"
p "xs[-1] should be 14 = #{xs[-1]}"
p "xs[100] should be nil = #{xs[100]}"

xs[20] = 20
p "xs.size should be 21 = #{xs.size}"
p "xs[20] should be 20 = #{xs[20]}"


ys = [1, 2, 3, 15, 16, 17, 18, 19]
foos = xs | ys
bars = xs + ys

p "foos=", foos
p "bars=", bars

p "Array.new(4) { 0 }", Array.new(4) { 0 }
p "Array.new(4) { |i| -i }", Array.new(4) { |i| -i }
