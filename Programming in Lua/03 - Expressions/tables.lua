-- These are all equal:
--
-- {x=0, y=0} == {["x"]=0, ["y"]=0}
-- {"red", "green", "blue"} == {[1]="red", [2]="green", [3]="blue"}


-- > Finally, you can always use a semicolon instead of a comma in a
-- constructor. We usually reserve semicolons to delimit different sections in
-- a constructor, for instance to separate its list part from its record part: 
--
--   {x=10, y=45; "one", "two", "three"}

days = {
  [0] = "Sunday",
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday"
}

-- Remmeber, ipairs skips [0] so we start on Monday
for i, day in ipairs(days) do
  print(i, day)
end

person = {
  name = "Marcos",
  age = 28,

  -- first item = [1]
  {
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  }
}

print(person.name)
print(person.age)

for i, day in ipairs(person[1]) do
  print(i, day)
end
