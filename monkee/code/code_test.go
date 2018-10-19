package code

import "testing"

func TestMake(t *testing.T) {
	tests := []struct {
		op       Optcode
		operands []int
		expected []byte
	}{
		{OpConstant, []int{65534}, []byte{byte(OpConstant), 255, 254}},
	}

	for _, tt := range tests {
		instruction := Make(tt.op, tt.operands...)

		if len(instruction) != len(tt.expected) {
			t.Errorf("instruction has wrong length, expected %d but got %d",
				tt.expected, len(instruction))
		}

		for i, b := range tt.expected {
			if instruction[i] != tt.expected[i] {
				t.Errorf("wrong byte at position %d, expected %d but got %d",
					i, b, instruction[i])
			}
		}
	}
}
