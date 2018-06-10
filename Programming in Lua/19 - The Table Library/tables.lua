-- The table library comprises aux functions to manupulate tables as arrays.
-- One of its main roles is to give a reasonable meaning for the size of an
-- array in Lua. It also provides functions to insert and remove elements from
-- lists and to sort the elements of an array.

-- table.getn and table.setn have been removed from the table object. See
-- https://www.lua.org/manual/5.1/manual.html. Use the new length "#" operator
-- instead.

-- The table.remove function removes (and returns) an element from a given
-- position in an array, moving down other elements to close space and
-- decrementing the size of the array. When called without a position, it
-- removes the last element of the array.

print("a = {}")
a = {}
print(#a)

print("b = {1, 2, 3}")
b = {1, 2, 3}
print(#b)

print("#{1;2}")
print(#{1;2})

print("table.insert(a, 123)")
table.insert(a, 123)
print(a[1])
print(#a)

-- remove at specific index and resize
print("table.remove(a, 1)")
table.remove(a, 1)
print(a[1])
print(#a)

-- push
table.insert(a, 1)
table.insert(a, 1)
table.insert(a, 1)
table.insert(a, 1)

-- pop
table.remove(a)
table.remove(a)
table.remove(a)
table.remove(a)

print(a[1])
