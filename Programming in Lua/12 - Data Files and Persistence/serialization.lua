-- use string.format to serialize string
function serialize (o, indent_level)
  if indent_level == nil then
    indent_level = 0
  end

  local indentation = string.rep(" ", indent_level)

  if type(o) == "number" then
    io.write(tostring(o))
  elseif type(o) == "string" then
    io.write(string.format("%q", o))
  elseif type(o) == "table" then
    io.write("{\n")

    for key, val in pairs(o) do
      io.write(indentation, "  [")
      serialize(key, 0)
      io.write("] = ")
      serialize(val, indent_level + 2)
      io.write(",\n")
    end

    io.write(indentation, "}")
  else
    error("unserializable type " .. type(o))
  end
end

serialize({
  {
    name = "Marcos Minond",
    age = 28,
    children = {
      {
        name = "Marcos Minond",
        age = 28,
      },
      {
        name = "Marcos Minond",
        age = 28,
      },
      {
        name = "Marcos Minond",
        age = 28,
      }
    }
  },
  {
    name = "Marcos Minond",
    age = 28,
  },
  {
    name = "Marcos Minond",
    age = 28,
  },
})
