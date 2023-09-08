# Nim types:
#
#   - int - same size as a pointer
#   - int8
#   - int16
#   - int32
#   - int64
#
#   - uint
#   - uint8
#   - uint16
#   - uint32
#   - uint64
#
#   - float - the processor's fastest type
#   - float32
#   - float64
#
#   - char - alias for uint8

let
  a: int8 = 0x7F
  b: uint8 = 0b0000_0010
  c: uint8 = 120

echo a
echo b
echo c
