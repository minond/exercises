op = "*"
left = 4
right = 2
result = nil

if op == "+" then
  result = left + right
elseif op == "-" then
  result = left - right
elseif op == "*" then
  result = left * right
elseif op == "/" then
  result = left / right
else
  error("Invalid operator: " .. op)
end

print(left .. " " .. op .. " " .. right .. " = " .. result)




print("while loop")
remaining = 10

while remaining > 0 do
  print(remaining)
  remaining = remaining - 1
end




print("repeat loop")
remaining = 10

repeat
  print(remaining)
  remaining = remaining - 1

  if remaining == 3 then
    break
  end
until remaining <= 0




-- for var = exp1, exp2, exp3 do
--   something
-- end
print("numeric for loop")
for i = 1,10 do
  print(i)
end




-- for i, v in ipairs(x) do
--   something
-- end
print("generic for loop")
days = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}
for i, day in ipairs(days) do
  print(i, day)
end
