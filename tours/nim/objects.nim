# Nim calls its structs "object". They are automatically garbage collected.
type
  Animal = object
    name, species: string
    age: int

  Thing = ref object
    x, y: int

proc sleep(a: var Animal) =
  a.age += 1

proc dead(a: Animal): bool =
  return a.age > 20

# Created on the stack with zero values, like Go.
var cat1 = Animal(name: "Carl",
                  species: "Cat",
                  age: 12)

# Also created on the stack but it is immutable
var cat2 = Animal(name: "Joe",
                  species: "Cat",
                  age: 8)

# Allocated on the heap. `cat3` is immutable but since it is a reference, the
# values it points to can change. Therefore this can be passed to a proc
# expecting a variable.
let cat3: ref Animal = new(Animal)
cat3.age = 5
cat3.name = "Testing"
cat3.species = "Cat"

for i in 0..100:
  cat1.sleep

echo "Is my cat1 dead? ", cat1.dead
echo "Is my cat2 dead? ", cat2.dead

# I'm not sure why I can't call the following:
# cat3.sleep
# cat3.dead
