-- Eight basic types in Lua:
--   nil
--   boolean
--   number
--   string
--   userdata
--   function
--   thread
--   table

print(type("Hi"))       -- string
print(type(12.54))      -- number
print(type(print))      -- function
print(type(type))       -- function
print(type(true))       -- boolean
print(type(nil))        -- nil
print(type(type(nil)))  -- string
