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
	case "StackMapTable":
		attr = &StackMapTableAttribute{}
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

type StackMapTableAttribute struct {
	AttributeNameIndex uint16
	AttributeLength    uint32
	NumberOfEntries    uint16
	Entries            []StackMapFrameEntry
}

func (attr *StackMapTableAttribute) Read(r *bufio.Reader, cp []CpInfo) {
	attr.AttributeNameIndex = read_u16(r)
	attr.AttributeLength = read_u32(r)
	attr.NumberOfEntries = read_u16(r)

	if attr.NumberOfEntries > 0 {
		attr.Entries = make([]StackMapFrameEntry, attr.NumberOfEntries)
		for i := uint16(0); i < attr.NumberOfEntries; i++ {
			var frame StackMapFrameEntry
			rawFrameType := peek_u8(r)

			switch frameTypeFromByte(rawFrameType) {
			case FullFrame:
				frame = &FullFrameStackMapFrame{}
			case Same:
				frame = &SameFrameStackMapFrame{}
			default:
				panic(fmt.Sprintf("unable to parse stack frame of type: %d", rawFrameType))
			}

			frame.Read(r)
		}
	}
}

type FrameType int

const (
	InvalidFrameType FrameType = iota
	Same
	SameLocals1StackItem
	SameLocals1StackItemExtended
	Chop
	SameFrameExtended
	Append
	FullFrame
)

func frameTypeFromByte(byte byte) FrameType {
	switch {
	case byte >= 0 && byte <= 63:
		return Same
	case byte >= 64 && byte <= 127:
		return SameLocals1StackItem
	case byte == 247:
		return SameLocals1StackItemExtended
	case byte >= 248 && byte <= 250:
		return Chop
	case byte == 251:
		return SameFrameExtended
	case byte >= 252 && byte <= 254:
		return Append
	case byte == 255:
		return FullFrame
	default:
		return InvalidFrameType
	}
}

type StackMapFrameEntry interface {
	Read(*bufio.Reader)
}

type SameFrameStackMapFrame struct {
	FrameType uint8
}

func (frame *SameFrameStackMapFrame) Read(r *bufio.Reader) {
	frame.FrameType = read_u8(r)
}

type FullFrameStackMapFrame struct {
	FrameType          uint8
	OffsetDelta        uint16
	NumberOfLocals     uint16
	Locals             []VerificationTypeInfoEntry
	NumberOfStackItems uint16
	Stack              []VerificationTypeInfoEntry
}

func (frame *FullFrameStackMapFrame) Read(r *bufio.Reader) {
	frame.FrameType = read_u8(r)
	frame.OffsetDelta = read_u16(r)

	frame.NumberOfLocals = read_u16(r)
	if frame.NumberOfLocals > 0 {
		frame.Locals = make([]VerificationTypeInfoEntry, frame.NumberOfLocals)
		for i := uint16(0); i < frame.NumberOfLocals; i++ {
			frame.Locals[i] = readVerificationTypeInfoEntry(r)
		}
	}

	frame.NumberOfStackItems = read_u16(r)
	frame.Stack = make([]VerificationTypeInfoEntry, frame.NumberOfStackItems)
	for i := uint16(0); i < frame.NumberOfStackItems; i++ {
		frame.Stack[i] = readVerificationTypeInfoEntry(r)
	}
}

func readVerificationTypeInfoEntry(r *bufio.Reader) VerificationTypeInfoEntry {
	var item VerificationTypeInfoEntry

	switch tag := peek_u8(r); ItemTag(tag) {
	case TopItem:
		item = &TopVariableInfo{}
	case IntegerItem:
		item = &IntegerVariableInfo{}
	case FloatItem:
		item = &FloatVariableInfo{}
	case DoubleItem:
		item = &DoubleVariableInfo{}
	case LongItem:
		item = &LongVariableInfo{}
	case NullItem:
		item = &NullVariableInfo{}
	case UninitializedThisItem:
		item = &UninitializedThisVariableInfo{}
	case ObjectItem:
		item = &ObjectVariableInfo{}
	case UninitializedItem:
		item = &UninitializedVariableInfo{}
	default:
		panic(fmt.Sprintf("unable to parse variable: %d", tag))
	}

	item.Read(r)
	return item
}

type ItemTag uint8

const (
	TopItem ItemTag = iota
	IntegerItem
	FloatItem
	DoubleItem
	LongItem
	NullItem
	UninitializedThisItem
	ObjectItem
	UninitializedItem
)

type VerificationTypeInfoEntry interface {
	Read(*bufio.Reader)
}

type TopVariableInfo struct{ Tag ItemTag }
type IntegerVariableInfo struct{ Tag ItemTag }
type FloatVariableInfo struct{ Tag ItemTag }
type DoubleVariableInfo struct{ Tag ItemTag }
type LongVariableInfo struct{ Tag ItemTag }
type NullVariableInfo struct{ Tag ItemTag }
type UninitializedThisVariableInfo struct{ Tag ItemTag }

type ObjectVariableInfo struct {
	Tag        ItemTag
	CpoolIndex uint16
}

type UninitializedVariableInfo struct {
	Tag    ItemTag
	Offset uint16
}

func (info *TopVariableInfo) Read(r *bufio.Reader)               { info.Tag = ItemTag(read_u8(r)) }
func (info *IntegerVariableInfo) Read(r *bufio.Reader)           { info.Tag = ItemTag(read_u8(r)) }
func (info *FloatVariableInfo) Read(r *bufio.Reader)             { info.Tag = ItemTag(read_u8(r)) }
func (info *DoubleVariableInfo) Read(r *bufio.Reader)            { info.Tag = ItemTag(read_u8(r)) }
func (info *LongVariableInfo) Read(r *bufio.Reader)              { info.Tag = ItemTag(read_u8(r)) }
func (info *NullVariableInfo) Read(r *bufio.Reader)              { info.Tag = ItemTag(read_u8(r)) }
func (info *UninitializedThisVariableInfo) Read(r *bufio.Reader) { info.Tag = ItemTag(read_u8(r)) }

func (info *ObjectVariableInfo) Read(r *bufio.Reader) {
	info.Tag = ItemTag(read_u8(r))
	info.CpoolIndex = read_u16(r)
}

func (info *UninitializedVariableInfo) Read(r *bufio.Reader) {
	info.Tag = ItemTag(read_u8(r))
	info.Offset = read_u16(r)
}
