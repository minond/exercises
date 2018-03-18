function producer ()
  return coroutine.create(function ()
    while true do
      print("DEBUG: Waiting in producer")
      coroutine.yield(io.read())                            -- Produce a new value
      print("DEBUG: Yielded in producer")
    end
  end)
end

function filter (prod)
  return coroutine.create(function ()
    local line = 1
    while true do
      print("DEBUG: Resuming in filter")
      local _, x = coroutine.resume(prod)                   -- Get new value
      coroutine.yield(string.format("%5d %s", line, x))     -- Sent it to consumer
      line = line + 1
    end
  end)
end

function consumer (prod)
  while true do
    print("DEBUG: Resuming in consumer")
    local _, x = coroutine.resume(prod)                     -- Get new value
    io.write(x, "\n")                                       -- Consume new value
  end
end

consumer(filter(producer()))
