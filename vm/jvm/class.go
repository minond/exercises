package main

import (
	"bufio"
	"encoding/hex"
	"encoding/json"
	"fmt"
)

type ClassFile struct {
	Magic             string
	MinorVersion      uint16
	MajorVersion      uint16
	ConstantPoolCount uint16
	ConstantPool      []CpInfo
	AccessFlags       uint16
	ThisClass         uint16
	SuperClass        uint16
	InterfacesCount   uint16
	Interfaces        []uint16
	FieldsCount       uint16
	Fields            []FieldInfo
	MethodsCount      uint16
	Methods           []MethodInfo
	AttributesCount   uint16
	Attributes        []AttributeInfo
}

func (cf ClassFile) String() string {
	bytes, err := json.MarshalIndent(cf, "", "    ")
	if err != nil {
		panic(fmt.Sprintf("unable to marshal class data: %s", err))
	}
	return string(bytes)
}

func Read(r *bufio.Reader) ClassFile {
	cf := ClassFile{}

	cf.Magic = hex.EncodeToString(read(r, 4))
	cf.MinorVersion = read_u16(r)
	cf.MajorVersion = read_u16(r)

	cf.ConstantPoolCount = read_u16(r)
	if cf.ConstantPoolCount > 0 {
		cf.ConstantPool = make([]CpInfo, cf.ConstantPoolCount-1)
		for i := uint16(0); i < cf.ConstantPoolCount-1; i++ {
			cf.ConstantPool[i] = readCpInfo(r)
		}
	}

	cf.AccessFlags = read_u16(r)
	cf.ThisClass = read_u16(r)
	cf.SuperClass = read_u16(r)

	cf.InterfacesCount = read_u16(r)
	if cf.InterfacesCount > 0 {
		cf.Interfaces = make([]uint16, cf.InterfacesCount)
		for i := uint16(0); i < cf.InterfacesCount; i++ {
			cf.Interfaces[i] = read_u16(r)
		}
	}

	cf.FieldsCount = read_u16(r)
	if cf.FieldsCount > 0 {
		cf.Fields = make([]FieldInfo, cf.FieldsCount)
		for i := uint16(0); i < cf.FieldsCount; i++ {
			cf.Fields[i] = readFieldInfo(r)
		}
	}

	cf.MethodsCount = read_u16(r)
	if cf.MethodsCount > 0 {
		cf.Methods = make([]MethodInfo, cf.MethodsCount)
		for i := uint16(0); i < cf.MethodsCount; i++ {
			cf.Methods[i] = readMethodInfo(r)
		}
	}

	cf.AttributesCount = read_u16(r)
	if cf.AttributesCount > 0 {
		cf.Attributes = make([]AttributeInfo, cf.AttributesCount)
		for i := uint16(0); i < cf.AttributesCount; i++ {
			cf.Attributes[i] = readAttributeInfo(r)
		}
	}

	return cf
}
