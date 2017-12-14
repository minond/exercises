plotter = require "./plotter"

do
  local mark = plotter.mark
  plotter.mark = function (x, y)
    mark(x+1, y+1)
  end
end

plotter.plot(function (x)
  return math.sin(x*2*math.pi)
end)
