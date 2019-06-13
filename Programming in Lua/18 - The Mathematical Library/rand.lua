math.randomseed(os.time())

-- random()          - [0, 1]
-- random(max)       - 1 <= x <= max
-- random(min, max)  - min <= x <= max

for i = 1, 10 do
  print("random()", math.random())
  print("random(10)", math.random(10))
  print("random(25, 75)", math.random(25, 75))
  print()
end
