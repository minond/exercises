-- Only `nil` and `false` are false values. Everything else is true.

if "" then
  print("An empty string is true")
end

if 0 then
  print("Zero is true")
end

if true then
  print("True is true")
end

if nil then
  print("Nil is true")
else
  print("Nil is not true")
end

if false then
  print("False is true")
else
  print("False is not true")
end
