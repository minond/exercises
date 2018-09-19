package main

import "fmt"

type Set interface {
	Push(item interface{})
	Cardinality() int
}

var Ø = EmptySet{}

type EmptySet struct{}

func (EmptySet) Push(_ interface{}) {}
func (EmptySet) Cardinality() int   { return 0 }

type ClassicalSet []interface{}

func (set *ClassicalSet) Push(item interface{}) { *set = append(*set, item) }
func (set ClassicalSet) Cardinality() (cardinality int) {
	unique := map[interface{}]struct{}{}
	for _, v := range set {
		unique[v] = struct{}{}
	}
	for range unique {
		cardinality++
	}
	return
}

func main() {
	states := ClassicalSet{Ø, Ø, "Utah", "Michigan", "Michigan"}
	fmt.Println(states.Cardinality())
	fmt.Println(Ø.Cardinality())
}
