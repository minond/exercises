print(os.date())
print [[one two three]]
print "one two three"

function print2(msg)
  print(msg)
end


myvar = "hi"
print2(myvar)



function f(x, y)
  print(x, y)
end


f(1)
f(1, 2)
f(1, 2, 3)


function defaults(x, y)
  x = x or 1
  y = y or 2
  return x * y
end

print(defaults(2, 5))
