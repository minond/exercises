package main

import (
	"encoding/json"
	"fmt"
)

type Class struct {
	Name string

	name       *Utf8Info
	methodrefs []*MethodrefInfo
	fieldrefs  []*FieldrefInfo
}

func NewClass(name *Utf8Info, methodrefs []*MethodrefInfo, fieldrefs []*FieldrefInfo, constantPool []CpInfo) *Class {
	return &Class{
		Name:       name.Value,
		name:       name,
		methodrefs: methodrefs,
		fieldrefs:  fieldrefs,
	}
}

func (class Class) String() string {
	bytes, err := json.MarshalIndent(class, "", "    ")
	if err != nil {
		panic(fmt.Sprintf("unable to marshal class: %s", err))
	}
	return string(bytes)
}
