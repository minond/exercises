package code

import (
	"fmt"
)

type Instructions []byte

type Optcode byte

type Definition struct {
	Name          string
	OperandWidths []int
}

const (
	OpConstant Optcode = iota
)

var definitions = map[Optcode]*Definition{
	OpConstant: {"OpConstant", []int{2}},
}

func Lookup(op byte) (*Definition, error) {
	def, ok := definitions[Optcode(op)]
	if !ok {
		return nil, fmt.Errorf("undefine opcode: %d", op)
	}

	return def, nil
}

func Make(op Optcode, operands ...int) []byte {
	def, ok := definitions[op]
	if !ok {
		return []byte{}
	}

	instructionLen := 1
	for _, w := range def.OperandWidths {
		instructionLen += w
	}

	instruction := make([]byte, instructionLen)
	instruction[0] = byte(op)

	offset := 1
	for i, o := range operands {
		width := def.OperandWidths[i]
		fmt.Printf("width at %d is %d\n", i, width)
		switch width {
		case 2:
			instruction[offset] = byte(o >> 8)
			instruction[offset+1] = byte(o)
		case 4:
			instruction[offset] = byte(o >> 24)
			instruction[offset+2] = byte(o >> 16)
			instruction[offset+2] = byte(o >> 8)
			instruction[offset+3] = byte(o)
		}

		offset += width
	}

	return instruction
}
