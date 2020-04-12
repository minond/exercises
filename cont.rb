require "continuation"
require "securerandom"

class Generator
  def initialize(&block)
    @gen_block = -> do
      begin
        block.call(self)
        @done = true
      rescue => e
        @error = e
        @done = true
      end
    end
  end

  def done?
    !!@done
  end

  def yield(val)
    callcc do |cc|
      @gen_cc = cc
      @ret_cc.call(val)
    end
  end

  def next
    raise @error if @error
    raise StandardError.new("generator is done") if @done
    ret = callcc do |cc|
      @ret_cc = cc
      @gen_cc.nil? ? @gen_block.call : @gen_cc.call
    end
    raise @error if @error
    ret
  end
end


gen = Generator.new do |ctx|
  100.times do |i|
    ctx.yield i
  end
end

def rangex
  counter = 0
  Generator.new do |ctx|
    loop do
      ctx.yield counter
      counter += 1
    end
  end
end

puts gen.next until gen.done?

x = rangex

1000.times { puts x.next }


def id_gen(make_id, counter_id, counter_step)
  Generator.new do |ctx|
    counter = counter_id
    loop do
      ctx.yield "#{counter}-#{make_id.call}"
      counter = counter.send(*counter_step)
      raise StandardError.new("A")
    end
  end
end

uuid = id_gen(-> { SecureRandom.uuid }, 1, [:+, 1])

1000.times { puts uuid.next }
