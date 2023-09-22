> A scalar type represents a single value. Rust has four primary scalar types:
> integers, floating-point numbers, Booleans, and characters.

> Length    Signed    Unsigned
> 8-bit     i8        u8
> 16-bit    i16       u16
> 32-bit    i32       u32
> 64-bit    i64       u64
> 128-bit   i128      u128
> arch      isize     usize

> Additionally, the isize and usize types depend on the architecture of the
> computer your program is running on, which is denoted in the table as “arch”

Number literals      Example
Decimal              98_222
Hex                  0xff
Octal                0o77
Binary               0b1111_0000
Byte (u8 only)       b'A'
