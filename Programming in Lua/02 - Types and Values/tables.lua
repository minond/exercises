a = {}
k = "x"
a[k] = 10
a[20] = "great"
print(a["x"])
k = 20
print(a[k])
a["x"] = a["x"] + 1
print(a["x"])
print(a)



a = {}
a["x"] = 10
b = a
print(b["x"])
b["x"] = 20
print(a["x"])
a = nil
print(b["x"])
b = nil
print(a, b)



a = {}
for i = 1, 1000 do
  a[i] = i*2
end
a["nine"] = a[9]
print(a[9])
print(a.nine)
print(a[9000])



a = {}
a[0] = "zero is 0"
a[1] = "one is 1"
a[2] = "two is 2"
-- a[3] = "three is 3"
a[4] = "four is 4"
a[5] = "five is 5"

-- The `ipairs` function starts at one and ends at the first nil values. So
-- this only prints out a[1] and a[2].
for idx, val in ipairs(a) do
  print(idx, val)
end



a = {}
a["0"] = "String zero"
a[0] = "Number zero"
print(a["0"], a[0])
