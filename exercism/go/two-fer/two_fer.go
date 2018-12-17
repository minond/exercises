// Package twofer helps you generate 2-fer messages
package twofer

// ShareWith returns a 2-fer message
func ShareWith(name string) string {
	if name == "" {
		name = "you"
	}
	return "One for " + name + ", one for me."
}
