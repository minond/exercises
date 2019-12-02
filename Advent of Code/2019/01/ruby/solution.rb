module_weights = ::File.read("./input.txt").split.map(&:to_i)

# Fuel required to launch a given module is based on its mass. Specifically, to
# find the fuel required for a module, take its mass, divide by three, round
# down, and subtract 2.
def required_fuel(x)
  (x / 3).round - 2
end

# The Fuel Counter-Upper needs to know the total fuel requirement. To find it,
# individually calculate the fuel needed for the mass of each module (your
# puzzle input), then add together all the fuel values.
part1 = module_weights.map { |x| required_fuel(x) }.inject(0, :+)

# Fuel itself requires fuel just like a module - take its mass, divide by
# three, round down, and subtract 2. However, that fuel also requires fuel, and
# that fuel requires fuel, and so on. Any mass that would require negative fuel
# should instead be treated as if it requires zero fuel; the remaining mass, if
# any, is instead handled by wishing really hard, which has no mass and is
# outside the scope of this calculation.
def fuel_for_fuel(weight)
  fuel = required_fuel(weight)
  fuel.positive? ? fuel + fuel_for_fuel(fuel) : 0
end

# What is the sum of the fuel requirements for all of the modules on your
# spacecraft when also taking into account the mass of the added fuel?
# (Calculate the fuel requirements for each module separately, then add them
# all up at the end.)
part2 = module_weights.map { |x| fuel_for_fuel(x) }.inject(0, :+)

puts "part 1: #{part1}"
puts "part 2: #{part2}"
