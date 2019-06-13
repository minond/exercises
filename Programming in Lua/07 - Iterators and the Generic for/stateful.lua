-- Iterators are simply functions that return their next value when called.
-- Return nil to stop the loop. They can be used in for loops like in the
-- example below.
function counter_with_next (max, step)
  if step == nil then
    step = 1
  end

  local current = 0
  local next = current + step

  return function ()
    current = current + step
    next = next + step

    if current < max then
      if next < max then
        return current, next
      else
        return current, nil
      end
    end
  end
end

-- Structure of for loops:
--
--   for <var-list> in <expr-list> do
--     <body>
--   end
for curr, next in counter_with_next(10) do
  print(curr, next)
end
