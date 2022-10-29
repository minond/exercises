package main

import (
	"bufio"
	"fmt"
)

type MethodInfo struct {
	AccessFlags     uint16
	NameIndex       uint16
	DescriptorIndex uint16
	AttributesCount uint16
	Attributes      []AttributeInfo
}

func (info *MethodInfo) Read(r *bufio.Reader, cp []CpInfo) {
	info.AccessFlags = read_u16(r)
	info.NameIndex = read_u16(r)
	info.DescriptorIndex = read_u16(r)
	info.AttributesCount = read_u16(r)
	if info.AttributesCount > 0 {
		info.Attributes = make([]AttributeInfo, info.AttributesCount)
		for i := uint16(0); i < info.AttributesCount; i++ {
			info.Attributes[i] = readAttribute(r, cp)
		}
	}
}

type FieldInfo struct {
	AccessFlags     uint16
	NameIndex       uint16
	DescriptorIndex uint16
	AttributesCount uint16
	Attributes      []AttributeInfo
}

func (info *FieldInfo) Read(r *bufio.Reader, cp []CpInfo) {
	info.AccessFlags = read_u16(r)
	info.NameIndex = read_u16(r)
	info.DescriptorIndex = read_u16(r)
	info.AttributesCount = read_u16(r)
	if info.AttributesCount > 0 {
		info.Attributes = make([]AttributeInfo, info.AttributesCount)
		for i := uint16(0); i < info.AttributesCount; i++ {
			info.Attributes[i] = readAttribute(r, cp)
		}
	}
}

const (
	Utf8Tag               uint8 = 1
	IntegerTag                  = 3
	FloatTag                    = 4
	LongTag                     = 5
	DoubleTag                   = 6
	ClassTag                    = 7
	StringTag                   = 8
	FieldrefTag                 = 9
	MethodrefTag                = 10
	InterfaceMethodrefTag       = 11
	NameAndTypeTag              = 12
	MethodHandleTag             = 15
	MethodTypeTag               = 16
	InvokeDynamicTag            = 18
)

type CpInfo interface {
	Read(*bufio.Reader)
}

func readCpInfo(r *bufio.Reader) CpInfo {
	var info CpInfo

	switch tag := peek_u8(r); tag {
	case Utf8Tag:
		info = &Utf8Info{}
	case ClassTag:
		info = &ClassInfo{}
	case StringTag:
		info = &StringInfo{}
	case FieldrefTag:
		info = &FieldrefInfo{}
	case MethodrefTag:
		info = &MethodrefInfo{}
	case NameAndTypeTag:
		info = &NameAndTypeInfo{}
	case MethodHandleTag:
		info = &MethodHandleInfo{}
	case InvokeDynamicTag:
		info = &InvokeDynamicInfo{}
	default:
		panic(fmt.Sprintf("unable to parse tag: %d", tag))
	}

	info.Read(r)
	return info
}

type ClassInfo struct {
	Name      string
	Tag       uint8
	NameIndex uint16
}

func (info *ClassInfo) Read(r *bufio.Reader) {
	info.Name = "CONSTANT_Class_info"
	info.Tag = read_u8(r)
	info.NameIndex = read_u16(r)
}

type MethodrefInfo struct {
	Name             string
	Tag              uint8
	ClassIndex       uint16
	NameAndTypeIndex uint16
}

func (info *MethodrefInfo) Read(r *bufio.Reader) {
	info.Name = "CONSTANT_Methodref_info"
	info.Tag = read_u8(r)
	info.ClassIndex = read_u16(r)
	info.NameAndTypeIndex = read_u16(r)
}

type NameAndTypeInfo struct {
	Name            string
	Tag             uint8
	NameIndex       uint16
	DescriptorIndex uint16
}

func (info *NameAndTypeInfo) Read(r *bufio.Reader) {
	info.Name = "CONSTANT_NameAndType"
	info.Tag = read_u8(r)
	info.NameIndex = read_u16(r)
	info.DescriptorIndex = read_u16(r)
}

type InvokeDynamicInfo struct {
	Name                     string
	Tag                      uint8
	BootstrapMethodAttrIndex uint16
	NameAndTypeIndex         uint16
}

func (info *InvokeDynamicInfo) Read(r *bufio.Reader) {
	info.Name = "CONSTANT_InvokeDynamic_info"
	info.Tag = read_u8(r)
	info.BootstrapMethodAttrIndex = read_u16(r)
	info.NameAndTypeIndex = read_u16(r)
}

type MethodHandleInfo struct {
	Name           string
	Tag            uint8
	ReferenceKind  uint8
	ReferenceIndex uint16
}

func (info *MethodHandleInfo) Read(r *bufio.Reader) {
	info.Name = "CONSTANT_MethodHandle_info"
	info.Tag = read_u8(r)
	info.ReferenceKind = read_u8(r)
	info.ReferenceIndex = read_u16(r)
}

type Utf8Info struct {
	Name   string
	Tag    uint8
	Length uint16
	Bytes  []byte
	Value  string
}

func (info *Utf8Info) Read(r *bufio.Reader) {
	info.Name = "CONSTANT_Utf8"
	info.Tag = read_u8(r)
	info.Length = read_u16(r)
	info.Bytes = read(r, uint32(info.Length))
	info.Value = string(info.Bytes)
}

type FieldrefInfo struct {
	Name             string
	Tag              uint8
	ClassIndex       uint16
	NameAndTypeIndex uint16
}

func (info *FieldrefInfo) Read(r *bufio.Reader) {
	info.Name = "CONSTANT_Fieldref"
	info.Tag = read_u8(r)
	info.ClassIndex = read_u16(r)
	info.NameAndTypeIndex = read_u16(r)
}

type StringInfo struct {
	Name        string
	Tag         uint8
	StringIndex uint16
}

func (info *StringInfo) Read(r *bufio.Reader) {
	info.Name = "CONSTANT_String"
	info.Tag = read_u8(r)
	info.StringIndex = read_u16(r)
}

type AttributeInfo interface {
	Read(*bufio.Reader, []CpInfo)
}

func readAttribute(r *bufio.Reader, cp []CpInfo) AttributeInfo {
	var attr AttributeInfo

	utf8, ok := cp[peek_u16(r)-1].(*Utf8Info)
	if !ok {
		panic("unable to locate utf-8 info constant for attribute")
	}

	switch utf8.Value {
	case "Code":
		attr = &CodeAttribute{}
	case "LineNumberTable":
		attr = &LineNumberTableAttribute{}
	case "SourceFile":
		attr = &SourceFileAttribute{}
	case "BootstrapMethods":
		attr = &BootstrapMethodsAttribute{}
	case "InnerClasses":
		attr = &InnerClassesAttribute{}
	default:
		panic(fmt.Sprintf("unable to parse attribute: %s", utf8.Value))
	}

	attr.Read(r, cp)
	return attr
}

type CodeAttribute struct {
	AttributeNameIndex   uint16
	AttributeLength      uint32
	MaxStack             uint16
	MaxLocals            uint16
	CodeLength           uint32
	Code                 []byte
	ExceptionTableLength uint16
	ExceptionTable       []*ExceptionTableEntry
	AttributesCount      uint16
	Attributes           []AttributeInfo
}

func (attr *CodeAttribute) Read(r *bufio.Reader, cp []CpInfo) {
	attr.AttributeNameIndex = read_u16(r)
	attr.AttributeLength = read_u32(r)
	attr.MaxStack = read_u16(r)
	attr.MaxLocals = read_u16(r)
	attr.CodeLength = read_u32(r)
	attr.Code = read(r, attr.CodeLength)

	attr.ExceptionTableLength = read_u16(r)
	if attr.ExceptionTableLength > 0 {
		attr.ExceptionTable = make([]*ExceptionTableEntry, attr.ExceptionTableLength)
		for i := uint16(0); i < attr.ExceptionTableLength; i++ {
			attr.ExceptionTable[i] = &ExceptionTableEntry{
				StartPc:   read_u16(r),
				EndPc:     read_u16(r),
				HandlerPc: read_u16(r),
				CatchType: read_u16(r),
			}
		}
	}

	attr.AttributesCount = read_u16(r)
	if attr.AttributesCount > 0 {
		attr.Attributes = make([]AttributeInfo, attr.AttributesCount)
		for i := uint16(0); i < attr.AttributesCount; i++ {
			attr.Attributes[i] = readAttribute(r, cp)
		}
	}
}

type ExceptionTableEntry struct {
	StartPc   uint16
	EndPc     uint16
	HandlerPc uint16
	CatchType uint16
}

type LineNumberTableAttribute struct {
	AttributeNameIndex    uint16
	AttributeLength       uint32
	LineNumberTableLength uint16
	LineNumberTable       []*LineNumberTableEntry
}

func (attr *LineNumberTableAttribute) Read(r *bufio.Reader, cp []CpInfo) {
	attr.AttributeNameIndex = read_u16(r)
	attr.AttributeLength = read_u32(r)
	attr.LineNumberTableLength = read_u16(r)
	if attr.LineNumberTableLength > 0 {
		attr.LineNumberTable = make([]*LineNumberTableEntry, attr.LineNumberTableLength)
		for i := uint16(0); i < attr.LineNumberTableLength; i++ {
			attr.LineNumberTable[i] = &LineNumberTableEntry{
				StartPc:    read_u16(r),
				LineNumber: read_u16(r),
			}
		}
	}
}

type LineNumberTableEntry struct {
	StartPc    uint16
	LineNumber uint16
}

type SourceFileAttribute struct {
	AttributeNameIndex uint16
	AttributeLength    uint32
	SourcefileIndex    uint16
}

func (attr *SourceFileAttribute) Read(r *bufio.Reader, cp []CpInfo) {
	attr.AttributeNameIndex = read_u16(r)
	attr.AttributeLength = read_u32(r)
	attr.SourcefileIndex = read_u16(r)
}

type BootstrapMethodsAttribute struct {
	AttributeNameIndex  uint16
	AttributeLength     uint32
	NumBootstrapMethods uint16
	BootstrapMethods    []*BootstrapMethodEntry
}

type BootstrapMethodEntry struct {
	BootstrapMethodRef    uint16
	NumBootstrapArguments uint16
	BootstrapArguments    []uint16
}

func (attr *BootstrapMethodsAttribute) Read(r *bufio.Reader, cp []CpInfo) {
	attr.AttributeNameIndex = read_u16(r)
	attr.AttributeLength = read_u32(r)
	attr.NumBootstrapMethods = read_u16(r)

	if attr.NumBootstrapMethods > 0 {
		attr.BootstrapMethods = make([]*BootstrapMethodEntry, attr.NumBootstrapMethods)
		for i := uint16(0); i < attr.NumBootstrapMethods; i++ {
			attr.BootstrapMethods[i] = &BootstrapMethodEntry{
				BootstrapMethodRef:    read_u16(r),
				NumBootstrapArguments: read_u16(r),
			}

			if attr.BootstrapMethods[i].NumBootstrapArguments > 0 {
				attr.BootstrapMethods[i].BootstrapArguments = make([]uint16, attr.BootstrapMethods[i].NumBootstrapArguments)
				for j := uint16(0); j < attr.BootstrapMethods[i].NumBootstrapArguments; j++ {
					attr.BootstrapMethods[i].BootstrapArguments[j] = read_u16(r)
				}
			}
		}
	}
}

type InnerClassesAttribute struct {
	AttributeNameIndex uint16
	AttributeLength    uint32
	NumberOfClasses    uint16
	Classes            []*InnerClassEntry
}

type InnerClassEntry struct {
	InnerClassInfoIndex   uint8
	OuterClassInfoIndex   uint8
	InnerNameIndex        uint8
	InnerClassAccessFlags uint8
}

func (attr *InnerClassesAttribute) Read(r *bufio.Reader, cp []CpInfo) {
	attr.AttributeNameIndex = read_u16(r)
	attr.AttributeLength = read_u32(r)
	attr.NumberOfClasses = read_u16(r)

	if attr.NumberOfClasses > 0 {
		attr.Classes = make([]*InnerClassEntry, attr.NumberOfClasses)
		for i := uint16(0); i < attr.NumberOfClasses; i++ {
			attr.Classes[i] = &InnerClassEntry{
				InnerClassInfoIndex:   read_u8(r),
				OuterClassInfoIndex:   read_u8(r),
				InnerNameIndex:        read_u8(r),
				InnerClassAccessFlags: read_u8(r),
			}
		}
	}
}
