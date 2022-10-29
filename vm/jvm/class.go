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
	Fields            []*FieldInfo
	MethodsCount      uint16
	Methods           []*MethodInfo
	AttributesCount   uint16
	Attributes        []AttributeInfo

	ClassInfosByIndex  map[uint16]*ClassInfo
	MethodsByNameIndex map[uint16][]*MethodInfo
	MethodRefsByIndex  map[uint16][]*MethodrefInfo
	FieldRefsByIndex   map[uint16][]*FieldrefInfo
}

func (cf ClassFile) String() string {
	bytes, err := json.MarshalIndent(cf, "", "    ")
	if err != nil {
		panic(fmt.Sprintf("unable to marshal class file: %s", err))
	}
	return string(bytes)
}

func (cf *ClassFile) Read(r *bufio.Reader) {
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
		cf.Fields = make([]*FieldInfo, cf.FieldsCount)
		for i := uint16(0); i < cf.FieldsCount; i++ {
			cf.Fields[i] = &FieldInfo{}
			cf.Fields[i].Read(r, cf.ConstantPool)
		}
	}

	cf.MethodsCount = read_u16(r)
	if cf.MethodsCount > 0 {
		cf.Methods = make([]*MethodInfo, cf.MethodsCount)
		for i := uint16(0); i < cf.MethodsCount; i++ {
			cf.Methods[i] = &MethodInfo{}
			cf.Methods[i].Read(r, cf.ConstantPool)
		}
	}

	cf.AttributesCount = read_u16(r)
	if cf.AttributesCount > 0 {
		cf.Attributes = make([]AttributeInfo, cf.AttributesCount)
		for i := uint16(0); i < cf.AttributesCount; i++ {
			cf.Attributes[i] = readAttribute(r, cf.ConstantPool)
		}
	}

	cf.cache()
}

func (cf *ClassFile) cache() {
	cf.ClassInfosByIndex = make(map[uint16]*ClassInfo)
	cf.MethodsByNameIndex = make(map[uint16][]*MethodInfo)
	cf.MethodRefsByIndex = make(map[uint16][]*MethodrefInfo)
	cf.FieldRefsByIndex = make(map[uint16][]*FieldrefInfo)

	for index, info := range cf.ConstantPool {
		switch ref := info.(type) {
		case *ClassInfo:
			cf.ClassInfosByIndex[uint16(index+1)] = ref
		case *MethodrefInfo:
			cf.MethodRefsByIndex[ref.ClassIndex] = append(cf.MethodRefsByIndex[ref.ClassIndex], ref)
		case *FieldrefInfo:
			cf.FieldRefsByIndex[ref.ClassIndex] = append(cf.FieldRefsByIndex[ref.ClassIndex], ref)
		}
	}

	for _, method := range cf.Methods {
		cf.MethodsByNameIndex[method.NameIndex] = append(cf.MethodsByNameIndex[method.NameIndex], method)
	}
}

func (cf ClassFile) Classes() ([]*Class, error) {
	var classes []*Class

	for index, classInfo := range cf.ClassInfosByIndex {
		nameInfo, ok := cf.ConstantPool[classInfo.NameIndex-1].(*Utf8Info)
		if !ok {
			return nil, fmt.Errorf("expecting class name utf8 info")
		}

		classes = append(classes, NewClass(nameInfo,
			cf.MethodRefsByIndex[index],
			cf.FieldRefsByIndex[index],
			cf.MethodsByNameIndex,
			cf))
	}

	return classes, nil
}

func (cf ClassFile) MethodByIndex(index int) *Method {
	ref, ok := cf.ConstantPool[index].(*MethodrefInfo)
	if !ok {
		return nil
	}

	nat, ok := cf.ConstantPool[ref.NameAndTypeIndex-1].(*NameAndTypeInfo)
	if !ok {
		return nil
	}

	name, ok := cf.ConstantPool[nat.NameIndex-1].(*Utf8Info)
	if !ok {
		return nil
	}

	impl := cf.MethodsByNameIndex[nat.NameIndex]
	return &Method{
		Name: name,
		Impl: impl,
	}
}
