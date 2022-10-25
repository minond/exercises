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

	methodRef   *MethodrefInfo
	nameAndType *NameAndTypeInfo
}

func (method Method) PrintInstructions() {
	for _, impl := range method.Impl {
		opcodes := bytes.NewReader(impl.Attributes[0].Info)

		for {
			opcode, err := opcodes.ReadByte()
			if err == io.EOF {
				break
			}

			// tail1, _ := opcodes.ReadByte()
			// tail2, _ := opcodes.ReadByte()
			// // tail3, _ := opcodes.ReadByte()
			// x := (opcode << 16) | (tail1 << 8) | (tail2 << 0)
			// fmt.Printf("    ?? 0x%0x\n", x)
			//
			// continue

			if mnemonic, found := instructionMnemonics[opcode]; found {
				fmt.Printf("    %s", mnemonic)
			} else {
				fmt.Printf("    ? 0x%0x", opcode)
			}

			switch opcode {
			case 0xb7:
				fallthrough
			case 0xb4:
				arg1, _ := opcodes.ReadByte()
				arg2, _ := opcodes.ReadByte()
				arg := (arg1 << 8) | arg2
				fmt.Printf(" %d", arg)
			}

			fmt.Println("")
		}
	}
}

type Field struct {
	Name *Utf8Info

	fieldRef    *FieldrefInfo
	nameAndType *NameAndTypeInfo
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
			Name:        name,
			Impl:        impl,
			methodRef:   ref,
			nameAndType: nat,
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
			Name:        name,
			fieldRef:    ref,
			nameAndType: nat,
		}
	}

	return &Class{
		Name:    name,
		Methods: methods,
		Fields:  fields,
	}
}
