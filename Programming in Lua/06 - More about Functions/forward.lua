function fact1 (n)
  if n == 0 then
    return 1
  else
    return n*fact1(n-1)
  end
end

-- Problem: fact2 technically points to a global fact2 function when this is
-- compiled:
--
--   local fact2 = function (n)
--     if n == 0 then
--       return 1
--     else
--       return n*fact2(n-1)
--     end
--   end
--
--
-- To get around this one can forward declare the function:
--
--   local fact2
--   fact2 = function (n)
--     if n == 0 then
--       return 1
--     else
--       return n*fact2(n-1)
--     end
--   end
--
-- Or use `local function` syntax instead:
local function fact2 (n)
  if n == 0 then
    return 1
  else
    return n*fact2(n-1)
  end
end


-- Forward declarations make this safe code:
--
--   local f, g
--
--   function g ()
--     f()
--   endj
--
--   function f ()
--     g()
--   end

print(fact1(4))
print(fact2(4))
