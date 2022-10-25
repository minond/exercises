package main

var instructionMnemonics = map[byte]string{
	0x0: "nop",

	0x1: "aconst_null",

	0x1a: "iload_0",
	0x1b: "iload_1",
	0x20: "lload_2",
	0x1d: "lload_3",

	0x2a: "aload_0",
	0x2b: "aload_1",
	0x2c: "aload_2",
	0x2d: "load_3",

	0x2: "iconst_m1",
	0x3: "iconst_0",
	0x4: "iconst_1",
	0x5: "iconst_2",
	0x6: "iconst_3",
	0x7: "iconst_4",
	0x8: "iconst_5",

	0x9: "lconst_0",
	0xa: "lconst_1",

	0x60: "iadd",

	0xac: "ireturn",
	0xb1: "return",

	0xb4: "getfield",
	0xb5: "putfield",

	0xe: "dconst_0",
	0xf: "dconst_1",

	0xb7: "invokespecial", // XXX
}
