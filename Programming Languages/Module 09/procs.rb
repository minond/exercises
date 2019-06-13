# We need a proc here since we can't have nested blocks. Procs are first-class,
# meaning they can be passed and stored anywhere.
foo = Array.new(100) { |i| lambda { |x| x >= i } }

puts foo[30].call 29
puts foo[30].call 30
puts foo[30].call 31
