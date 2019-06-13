-- Strings in Lua are immutable. The string.sub function, like any other
-- function in Lua, does not change the value of a string, but returns a new
-- string.

print(string.len("123"))
print(string.rep("a", 10))

function isortstring (strs)
  table.sort(strs, function (l, r)
    return string.lower(l) < string.lower(r)
  end)

  return strs
end

for i, str in ipairs(isortstring({"a", "z", "B"})) do
  print(i, str)
end


s = "[in brackets]"
print("string.sub(s, 2)", string.sub(s, 2))
print("string.sub(s, -2)", string.sub(s, -2))
print("string.sub(s, 0, -2)", string.sub(s, 0, -2))
print("string.sub(s, 2, -2)", string.sub(s, 2, -2))


-- string.format is like printf in C. It's composed of regular text and
-- directives that control where and how each argument must be placed in the
-- formatted string:
--
-- - %d: decimal number
-- - %x: hexadecimal
-- - %o: octal
-- - %f: floating point
-- - %s: strings
--
-- Betwee the % and the letter a directives can include other options

print(string.format("pi = %.4f", math.pi))

d = 5
m = 11
y = 1990
print(string.format("%02d/%02d/%04d", d, m, y))
