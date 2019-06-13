local defaultHeight = 24
local defaultWidth = 80

-- Could be any of the following:
--
--   plotter = {}
--   plotter.clear = function () end
--   plotter.etc = ...
--
--
--   plotter = {}
--   function plotter.clear () end
--   function plotter.etc ...
--
-- Or the could all go in the constructor, like this:
plotter = {
  clear = function ()
    print("\27[2J")
  end,

  read = function ()
    return io.read()
  end,

  mark = function (x, y)
    io.write(string.format("\27[%d;%dH*", y, x))
  end,

  plot = function (fn, w, h)
    plotter.clear()

    if w == nil then
      w = defaultWidth
    end

    if h == nil then
      h = defaultHeight
    end

    for i = 1, w do
      local x = (i/w)*2 - 1
      local y = (fn(x)+1) / 2*h
      plotter.mark(i, y)
    end

    plotter.read()
  end
}

return plotter
