# Enum values are  prepended with identifiers to avoid conflicts. The
# `{.pure.}` pragma requires all `Colors` values be qualified making the prefix
# unnecessary.
type
  CompassDirection = enum
    cdNorth,
    cdEast,
    cdSouth,
    cdWest

  Colors {.pure.} = enum
    Red = "FF0000",
    Green = (1, "00FF00"),
    Blue = "0000FF"

  Signals = enum
    Quit = 3,
    Abort = 6,
    Kill = 9

for direction in ord(low(CompassDirection)) .. ord(high(CompassDirection)):
  echo CompassDirection(direction), " ord: ", direction

# Because enums are ordinals, they have:
#   - low - which gives the lowest possible value
#   - high - which give the highest possible value
#   - inc - which increments the value
#   - dec - which decrements the value
#   - ord - which gives the integer value of the enum
#   - CompassDirection is a cast that taken an int and returns a CompassDirection
