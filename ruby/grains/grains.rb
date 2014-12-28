class Grains
  def square(index)
    2 ** (index - 1)
  end

  def total
    square(65) - 1
  end
end
