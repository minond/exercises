xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]

puts "xs.size should be 15\t\t= #{xs.size}"
puts "xs[0] should be 0\t\t= #{xs[0]}"
puts "xs[-1] should be 14\t\t= #{xs[-1]}"
puts "xs[100] should be nil\t\t= #{xs[100]}"

xs[20] = 20
puts "xs.size should be 21\t\t= #{xs.size}"
puts "xs[20] should be 20\t\t= #{xs[20]}"


ys = [1, 2, 3, 15, 16, 17, 18, 19]
foos = xs | ys
bars = xs + ys

puts "foos=", foos
puts "bars=", bars

puts "Array.new(4) { 0 }", Array.new(4) { 0 }
puts "Array.new(4) { |i| -i }", Array.new(4) { |i| -i }
