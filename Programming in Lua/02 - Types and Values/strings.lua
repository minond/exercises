-- Strings are immutable

a = "one string"
b = string.gsub(a, "one", "another")

page = [[
<html>
  <head>
    <title>Hi, from Lua!</title>
  </head>
</html>
]]

print(a)
print(b)

print(page)

-- Strings are automatically converted into number
print(10 .. 1)      -- 11
print("10" + 1)     -- 11
print("10" + "3")   -- 13

-- And numbers are automatically converted into strings
print(10 .. 20)     -- 1020
print("10" .. 1)    -- 101
print("10" .. "3")  -- 103

-- Convert string to number: tonumber
-- Convert number to string: tostring
print(10 == "10")             -- false
print(tostring(10) == "10")   -- true
print(10 .. "" == "10")       -- true

line = io.read()
n = tonumber(line)

if n == nil then
  error(line .. " is not a valid number")
else
  print(n * 2)
end
