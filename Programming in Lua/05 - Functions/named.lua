function gui (opts)
  if type(opts.name) ~= "string" then
    error("Missing opts.name option")
  elseif type(opts.width) ~= "number" then
    error("Missing opts.width option")
  elseif type(opts.height) ~= "number" then
    error("Missing opts.height option")
  end

  print(opts.name)
  print(opts.width)
  print(opts.height)
end


gui({ name = "M", width = 50, height = 36 })
