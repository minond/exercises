function counter (start)
  local tick = start

  return function (step)
    if step == nil then
      step = 1
    end

    return function ()
      tick = tick + step
      return tick
    end
  end
end

from10every2 = counter(10)(2)

for i = 0, 10 do
  print(i, from10every2())
end
