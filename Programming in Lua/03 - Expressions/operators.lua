-- Arithmetic operators:
--   + addtion
--   - subtraction
--   * multiplication
--   / division
--   - negation
--   ^ exponential



-- Relational operators:
--   <  less than
--   >  greater than
--   <= less than or eq
--   >= greater than or eq
--   == equal (with type equality)
--   ~= not equal to

-- Tables, userdata, and functions are compuared by reference
a = {}
b = {}
c = a

print(a == b) -- false
print(b == c) -- false
print(a == c) -- true

-- Strings are compared in alphabetical order. Comparing strings to numbers
-- raises an error.
print("acai" < "açaí")



-- Logical operators:
--   and
--   or
--   not
print(4 and 21)
print(nil and 2)
print((true and "hi") or "bye") -- same as `true ? "hi" : "bye"
