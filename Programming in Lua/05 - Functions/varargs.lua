function pp (a, b, ...)
  local arg = {...} -- or ipairs({...}), ipairs{...}

  print(a)
  print(b)

  for i, val in ipairs(arg) do
    print(val)
  end
end

pp(1, 2, 3, 4, 5, 6, 7)
