-- Find: string.find
-- Global find: string.gmatch
-- Global substitution: string.gsub

song = [[
[Verse 1]
Something in the way she moves
Attracts me like no other lover
Something in the way she woos me
I don't want to leave her now
You know I believe and how

[Verse 2]
Somewhere in her smile she knows
That I don't need no other lover
Something in her style that shows me
I don't want to leave her now
You know I believe and how

[Bridge]
You're asking me will my love grow
I don't know, I don't know
You stick around and it may show
I don't know, I don't know
]]


print(song)
print([[Matching "Something" in song:]])

loc = 0
while true do
  loc, to = string.find(song, "Something", loc+1)
  if loc == nil then break end
  print(loc, to)
end

print()
print([[Replacing "Something" with "Blah" two times:]])
print(string.gsub(song, "Something", "Blah", 2))
