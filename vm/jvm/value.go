package main

type Class struct {
	Name    string
	Methods []*Method
	Fields  []*Field
}

type Method struct {
	Name *Utf8Info
	Impl []*MethodInfo

	methodRef   *MethodrefInfo
	nameAndType *NameAndTypeInfo
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
		Name:    name.Value,
		Methods: methods,
		Fields:  fields,
	}
}
