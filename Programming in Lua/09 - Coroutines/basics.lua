co1 = coroutine.create(function ()
  print("Printing from function")
end)

print("Status of co1:", coroutine.status(co1))
print(coroutine.resume(co1))
print(coroutine.resume(co1))
print(coroutine.resume(co1))
print(coroutine.resume(co1))
print("Status of co1:", coroutine.status(co1))


co2 = coroutine.create(function (from, to)
  for i = from, to do
    print("Printing " .. i .. " from loop in function")
    coroutine.yield(i, i * i)
  end

  return to+1, (to+1) * (to+1)
end)

print("Status of co2:", coroutine.status(co2))
print(coroutine.resume(co2, 1, 9))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print(coroutine.resume(co2))
print("Status of co2:", coroutine.status(co2))

print(co1, co2)
