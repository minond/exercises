foo = Array.new(5) { |i| i }
bar = foo.map { |i| i * i }

# Synonyms:
# - map: collect
# - filter: select
# - reduce: inject

p foo
p bar
p foo.any? { |i| i > 15 }
p bar.any? { |i| i > 15 }
p bar.inject(0) { |acc, x| acc + x }

def t i
  (0..i).each do |j|
    print "  " * j
    (j..i).each do |k|
      print k
      print " "
    end

    print "\n"
  end
end

puts
t 9
