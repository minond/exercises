type Animal = ref object of RootObj
  name: string
  age: int

method vocalize(this: Animal): string {.base.} = "..."
method ageInHumanYears(this: Animal): int {.base.} = this.age


type Dog = ref object of Animal
method vocalize(this: Dog): string = "woof"
method ageInHumanYears(this: Dog): int = this.age * 7


type Cat = ref object of Animal
method vocalize(this: Cat): string = "meow"

let animals = @[Dog(name: "Sparky", age: 10), Cat(name: "Kitty", age: 7)]

for animal in animals:
  echo animal.vocalize
  echo animal.age
  echo animal.ageInHumanYears

  echo(animal of Animal)
  echo(animal of Dog)
  echo(animal of Cat)
