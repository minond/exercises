-- Naturals: Essentially returns { last = 0 } with a loopup table of `self`,
-- which only really works because `self` has an index loopup, which is also
-- `self`, allowing us to create new tables (instances) that have lookups all
-- the way back to `Naturals`.
Naturals = {}

function Naturals:new ()
  self.__index = self
  return setmetatable({ last = 0 }, self)
end

function Naturals:next ()
  return self.last + 1
end

function Naturals:forward (n)
  for i = 1, n do
    self.last = self:next()
    print(self.last)
  end
end


-- Squares: Just like Naturals, Squares is also a table, except we're starting
-- with the table we get back from a call to `Naturals:new`. By it self, this
-- is just like any other "instance" of a Naturals so we could technically use
-- it as such, but instead we'll use to as a child class and override the
-- `next` method. Later, when Squares is used with `Squares:new`, the call to
-- `new` will reference `Naturals:new` *but* where `self` will be `Squares`
-- instead, allowing us to intercept calls to `Naturals:next` by declaring
-- `Squares:next` which comes earlier in the lookup table sequence.
Squares = Naturals:new()

function Squares:next ()
  if self.last == 0 then
    self.last = 2
  end

  return self.last * self.last
end


seq_nats = Naturals:new()
seq_nats:forward(5)

seq_seq_nats = seq_nats:new()
seq_seq_nats:forward(5)

seq_seq_seq_nats = seq_seq_nats:new()
seq_seq_seq_nats:forward(5)

seq_seq_seq_seq_nats = seq_seq_seq_nats:new()
seq_seq_seq_seq_nats:forward(5)

seq_sqrs = Squares:new()
seq_sqrs:forward(5)
