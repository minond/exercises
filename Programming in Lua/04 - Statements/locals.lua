x = 2
print(x)

do
  -- Still looking at parent's scope
  print(x)

  -- Now we're local to the chunk
  local x = 3 ; print(x)
end

print(x)
