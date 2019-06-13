lines = {
  luaH_set = 10,
  luaH_get = 24,
  luaH_present = 48,
}

tmp = {}
for n in pairs(lines) do
  table.insert(tmp, n)
end

table.sort(tmp)

for i, n in pairs(tmp) do
  print(n, lines[n])
end


function sortedkeypairs (t, fn)
  local keys = {}

  for key in pairs(t) do
    table.insert(keys, key)
  end

  table.sort(keys, f)

  local i = 0
  local iter = function ()
    i = i + 1
    if keys[i] == nil then
      return nil
    else
      return keys[i], t[keys[i]]
    end
  end

  return iter
end

for key, val in sortedkeypairs({a = 321, z = "ZZ", b = 9}) do
  print(key, val)
end
