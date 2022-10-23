package main

import (
	"bufio"
	"encoding/hex"
	"encoding/json"
	"fmt"
)

type Class struct {
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

func (class Class) String() string {
	bytes, err := json.MarshalIndent(class, "", "    ")
	if err != nil {
		panic(fmt.Sprintf("unable to marshal class data: %s", err))
	}
	return string(bytes)
}

func Read(r *bufio.Reader) Class {
	class := Class{}

	class.Magic = hex.EncodeToString(read(r, 4))
	class.MinorVersion = read_u16(r)
	class.MajorVersion = read_u16(r)

	class.ConstantPoolCount = read_u16(r)
	if class.ConstantPoolCount > 0 {
		class.ConstantPool = make([]CpInfo, class.ConstantPoolCount-1)
		for i := uint16(0); i < class.ConstantPoolCount-1; i++ {
			class.ConstantPool[i] = readCpInfo(r)
		}
	}

	class.AccessFlags = read_u16(r)
	class.ThisClass = read_u16(r)
	class.SuperClass = read_u16(r)

	class.InterfacesCount = read_u16(r)
	if class.InterfacesCount > 0 {
		class.Interfaces = make([]uint16, class.InterfacesCount)
		for i := uint16(0); i < class.InterfacesCount; i++ {
			class.Interfaces[i] = read_u16(r)
		}
	}

	class.FieldsCount = read_u16(r)
	if class.FieldsCount > 0 {
		class.Fields = make([]FieldInfo, class.FieldsCount)
		for i := uint16(0); i < class.FieldsCount; i++ {
			class.Fields[i] = readFieldInfo(r)
		}
	}

	class.MethodsCount = read_u16(r)
	if class.MethodsCount > 0 {
		class.Methods = make([]MethodInfo, class.MethodsCount)
		for i := uint16(0); i < class.MethodsCount; i++ {
			class.Methods[i] = readMethodInfo(r)
		}
	}

	class.AttributesCount = read_u16(r)
	if class.AttributesCount > 0 {
		class.Attributes = make([]AttributeInfo, class.AttributesCount)
		for i := uint16(0); i < class.AttributesCount; i++ {
			class.Attributes[i] = readAttributeInfo(r)
		}
	}

	return class
}