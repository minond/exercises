-- Stateless iterators do not keep track of their own states. They instead rely
-- on the arguments passed to it by the for loop, which include an invariant
-- state and control variable. Here's a lua implementation of the ipairs
-- function:
function ipairs_2_inner (xs, i)
  local idx = i + 1
  local val = xs[idx]

  if val then
    return idx, val
  end
end

function iparis_2 (xs)
  return ipairs_2_inner, xs, 0
end


-- Notice below how iparis_2 is called as a function and later ipairs_2_inner,
-- the immutable state (xs) and the control variable (in this case 0) are
-- directly used in the for loop statement.
xs = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}

for i, val in iparis_2(xs) do
  print(i, val)
end

for i, val in ipairs_2_inner, xs, 0 do
  print(i, val)
end
