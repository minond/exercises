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

func readMethodInfo(r *bufio.Reader) MethodInfo {
	info := MethodInfo{}
	info.AccessFlags = read_u16(r)
	info.NameIndex = read_u16(r)
	info.DescriptorIndex = read_u16(r)
	info.AttributesCount = read_u16(r)
	if info.AttributesCount > 0 {
		info.Attributes = make([]AttributeInfo, info.AttributesCount)
		for i := uint16(0); i < info.AttributesCount; i++ {
			info.Attributes[i] = readAttributeInfo(r)
		}
	}
	return info
}

type FieldInfo struct {
	AccessFlags     uint16
	NameIndex       uint16
	DescriptorIndex uint16
	AttributesCount uint16
	Attributes      []AttributeInfo
}

func readFieldInfo(r *bufio.Reader) FieldInfo {
	info := FieldInfo{}
	info.AccessFlags = read_u16(r)
	info.NameIndex = read_u16(r)
	info.DescriptorIndex = read_u16(r)
	info.AttributesCount = read_u16(r)
	if info.AttributesCount > 0 {
		info.Attributes = make([]AttributeInfo, info.AttributesCount)
		for i := uint16(0); i < info.AttributesCount; i++ {
			info.Attributes[i] = readAttributeInfo(r)
		}
	}
	return info
}

type AttributeInfo struct {
	AttributeNameIndex uint16
	AttributeLength    uint32
	Info               []byte
}

func readAttributeInfo(r *bufio.Reader) AttributeInfo {
	info := AttributeInfo{}
	info.AttributeNameIndex = read_u16(r)
	info.AttributeLength = read_u32(r)
	info.Info = read(r, info.AttributeLength)
	return info
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
