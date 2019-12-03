def load_opcodes
  orig ||= ::File.read("./input.txt").split(",")
  orig.clone
end

# An Intcode program is a list of integers separated by commas (like
# 1,0,0,3,99). To run one, start by looking at the first integer (called
# position 0). Here, you will find an opcode - either 1, 2, or 99. The opcode
# indicates what to do;
#
# - Opcode 1 adds together numbers read from two positions and stores the
#   result in a third position. The three integers immediately after the opcode
#   tell you these three positions - the first two indicate the positions from
#   which you should read the input values, and the third indicates the
#   position at which the output should be stored.
#
# - Opcode 2 works exactly like opcode 1, except it multiplies the two inputs
#   instead of adding them. Again, the three integers after the opcode indicate
#   where the inputs and outputs are, not their values.
#
# - Opcode 99 means that the program is finished and should immediately halt.
#
# Once you're done processing an opcode, move to the next one by stepping
# forward 4 positions. Encountering an unknown opcode means something went
# wrong.
class IntcodeComputer
  OPCODE_ADD = 1
  OPCODE_MULT = 2
  OPCODE_HALT = 99

  HANDLERS = {
    OPCODE_ADD  => [4, proc { write(deref(3), param(1) + param(2)) }],
    OPCODE_MULT => [4, proc { write(deref(3), param(1) * param(2)) }],
  }

  # @param [String, Array] code
  def initialize(code)
    @ok = true
    @pc = 0

    if code.is_a?(String)
      code = code.split(",")
    end

    @memory = code.map(&:to_i)
  end

  # Process the program to completion
  def compute
    step until done?
  end

  # Read from memory at any given position
  #
  # @param [Int] pos
  # @return [Int]
  def read(pos)
    memory[pos]
  end

  def to_s
    memory.join(",")
  end

private

  attr_reader :memory, :pc, :ok

  def done?
    opcode == OPCODE_HALT || !ok
  end

  # Write to memory at any given position
  #
  # @param [Int] pos
  def write(pos, val)
    memory[pos] = val
  end

  # Get the current opcode
  #
  # @return [Int]
  def opcode
    read(pc)
  end

  # Load value of an instruction's parameter at a given offset from the pc
  #
  # @param [Int] offset
  # @return [Int]
  def param(offset)
    read(deref(offset))
  end

  # Dereferences an address at a given offset from the pc
  #
  # @param [Int] offset
  # @return [Int]
  def deref(offset)
    memory[pc + offset]
  end

  # Handle the current opcode
  def step
    unless HANDLERS.has_key?(opcode)
      @ok = false
      return
    end

    size, handler = HANDLERS[opcode]
    instance_eval &handler
    @pc += size
  end
end

# Once you have a working computer, the first step is to restore the gravity
# assist program (your puzzle input) to the "1202 program alarm" state it had
# just before the last computer caught fire. To do this, before running the
# program, replace position 1 with the value 12 and replace position 2 with the
# value 2. What value is left at position 0 after the program halts?
opcodes = load_opcodes
opcodes[1] = 12
opcodes[2] = 2

comp = IntcodeComputer.new(opcodes)
comp.compute
puts "part1: #{comp.read(0)}"

# "With terminology out of the way, we're ready to proceed. To complete the
# gravity assist, you need to determine what pair of inputs produces the output
# 19690720."
#
# The inputs should still be provided to the program by replacing the values at
# addresses 1 and 2, just like before. In this program, the value placed in
# address 1 is called the noun, and the value placed in address 2 is called the
# verb. Each of the two input values will be between 0 and 99, inclusive.
#
# Find the input noun and verb that cause the program to produce the output
# 19690720. What is 100 * noun + verb? (For example, if noun=12 and verb=2, the
# answer would be 1202.)
expected = 19690720
$noun = nil
$verb = nil

def with_input_permutations
  (0..99).each do |noun|
    (0..99).each do |verb|
      yield(noun, verb)
    end
  end
end

with_input_permutations do |noun, verb|
  opcodes = load_opcodes
  opcodes[1] = noun
  opcodes[2] = verb

  comp = IntcodeComputer.new(opcodes)
  comp.compute

  if comp.read(0) == expected
    $noun = noun
    $verb = verb
    break
  end
end

puts "part2: #{100 * $noun + $verb}"
