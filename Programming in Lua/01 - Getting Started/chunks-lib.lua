-- To load this:
--   $ lua
--   > dofile("chunks-lib.lua")
--   > n = norm(2.4, 1.0)
--   > print(twice(n))

function norm (x, y)
  local n2 = x^2 + y^2
  return math.sqrt(n2)
end

function twice (x)
  return 2*x
end
