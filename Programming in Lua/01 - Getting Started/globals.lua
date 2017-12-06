-- Global vars don't need declarations. Using an unassigned variables results
-- in `nil`. Assign globals back to `nil` to delete them.

print(b) -- nil
b = 123
print(b) -- 123
b = nil
print(b) -- nil
