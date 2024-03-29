package main

import (
	"bytes"
	"fmt"
	"io"
)

type Class struct {
	Name    *Utf8Info
	Methods []*Method
	Fields  []*Field
}

type Method struct {
	Name *Utf8Info
	Impl []*MethodInfo
}

func (method Method) PrintInstructions() {
	for _, impl := range method.Impl {
		for _, attr := range impl.Attributes {
			code, ok := attr.(*CodeAttribute)
			if !ok {
				continue
			}

			opcodes := bytes.NewReader(code.Code)
			for {
				opcode, err := opcodes.ReadByte()
				if err == io.EOF {
					break
				}

				if mnemonic, found := instructionMnemonics[opcode]; found {
					fmt.Printf("    0x%02x		%s", opcode, mnemonic)
				} else {
					fmt.Printf("    0x%02x		?", opcode)
				}

				switch opcode {
				case 0x12:
					fallthrough
				case 0x16:
					fallthrough
				case 0x10:
					arg, _ := opcodes.ReadByte()
					fmt.Printf(" %d", arg)

				case 0xa7:
					fallthrough
				case 0xb5:
					fallthrough
				case 0xbb:
					fallthrough
				case 0xb2:
					fallthrough
				case 0xb6:
					fallthrough
				case 0xb7:
					fallthrough
				case 0xb4:
					arg1, _ := opcodes.ReadByte()
					arg2, _ := opcodes.ReadByte()
					arg := (arg1 << 8) | arg2
					fmt.Printf(" #%d", arg)

				case 0xba:
					arg1, _ := opcodes.ReadByte()
					arg2, _ := opcodes.ReadByte()
					_, _ = opcodes.ReadByte()
					_, _ = opcodes.ReadByte()
					arg := (arg1 << 8) | arg2
					fmt.Printf(" #%d 0 0", arg)
				}

				fmt.Println("")
			}
		}
	}
}

type Field struct {
	Name *Utf8Info
}

func NewClass(name *Utf8Info, methodRefsByIndex []*MethodrefInfo, fieldRefsByIndex []*FieldrefInfo, methodsByNameIndex map[uint16][]*MethodInfo, cf ClassFile) *Class {
	methods := make([]*Method, len(methodRefsByIndex))
	for i, ref := range methodRefsByIndex {
		nat, ok := cf.ConstantPool[ref.NameAndTypeIndex-1].(*NameAndTypeInfo)
		if !ok {
			panic("unable to access method NameAndTypeInfo")
		}

		name, ok := cf.ConstantPool[nat.NameIndex-1].(*Utf8Info)
		if !ok {
			panic("unable to access method NameAndTypeInfo.Name")
		}

		impl := methodsByNameIndex[nat.NameIndex]
		methods[i] = &Method{
			Name: name,
			Impl: impl,
		}
	}

	fields := make([]*Field, len(fieldRefsByIndex))
	for i, ref := range fieldRefsByIndex {
		nat, ok := cf.ConstantPool[ref.NameAndTypeIndex-1].(*NameAndTypeInfo)
		if !ok {
			panic("unable to access field NameAndTypeInfo")
		}

		name, ok := cf.ConstantPool[nat.NameIndex-1].(*Utf8Info)
		if !ok {
			panic("unable to access field NameAndTypeInfo.Name")
		}

		fields[i] = &Field{
			Name: name,
		}
	}

	return &Class{
		Name:    name,
		Methods: methods,
		Fields:  fields,
	}
}
