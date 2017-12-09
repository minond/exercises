function multipleReturns (x, y, z)
  return x*2, y*3, z*4
end

print(multipleReturns(3, 5, 2))


function largestTwo (xs)
  local largest = 0
  local secondLargest = 0

  for i, val in ipairs(xs) do
    if val > largest then
      secondLargest = largest
      largest = val
    elseif val > secondLargest then
      secondLargest = val
    end
  end

  return largest, secondLargest
end


print(largestTwo({1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))
print(largestTwo({10, 9, 8, 7, 6, 5, 4, 3, 2, 1}))

-- From https://www.lua.org/pil/5.1.html:
-- > When the call to foo2 appears inside an expression, Lua adjusts the number
-- > number of results to one; so, in the last line, only the "a" is used in
-- > the concatenation.
print(largestTwo({10, 9, 8, 7, 6, 5, 4, 3, 2, 1}) .. "X")


largest, secondLargest = largestTwo({10, 9, 8, 7, 6, 5, 4, 3, 2, 1})
print(largest, secondLargest)


print({1, 2, 3, 4})
print(unpack{1, 2, 3, 4})

function arrList (max)
  if max > 0 then
    return max, arrList(max - 1)
  end
end

four = arrList(4)
four, three, two, one = arrList(4)
print(four, three, two, one)
print(arrList(4))



-- Lua version of the unpack function:
function unpack (xs, i)
  i = i or 1

  if xs[i] ~= nil then
    return xs[i], unpack(xs, i + 1)
  end
end

print({1, 2, 3, 4})
print(unpack{1, 2, 3, 4})
