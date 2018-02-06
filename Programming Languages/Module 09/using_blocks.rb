def pass x
  yield x
end

pass(10) { |x| puts "Got #{x}" }
pass("hi") { |x| puts "Got #{x}" }
pass(true) { |x| puts "Got #{x}" }

def gb
  puts "Got block = #{block_given?}"
end

gb
gb { t }
