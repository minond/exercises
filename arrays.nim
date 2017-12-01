type
  ThreeStringAddress = array[3, string]

  PartsOfSpeech {.pure.} = enum
    Pronoun, Verb, Article, Adjective, Noun, Adverb

  Matrix[W, H: static[int]] =
    array[1..W, array[1..H, int]]

let
  names1: ThreeStringAddress = ["One", "Two", "Three"]
  names2: ThreeStringAddress = ["Four", "Five", "Six"]

  partOfSpeechExamples: array[PartsOfSpeech, string] = [
    "he", "reads", "the", "green", "book", "slowly"
  ]

  mat1: Matrix[3, 3] = [[1, 0, 1],
                        [0, 1, 1],
                        [1, 1, 0]]

  mat2: Matrix[3, 3] = [[1, 0, 1],
                        [0, 1, 1],
                        [1, 1, 0]]

proc zip[I, T](a, b: array[I, T]): array[I, tuple[a, b: T]] =
  for i in low(a) .. high(a):
    result[i] = (a[i], b[i])

proc `$`[W, H](mat: Matrix[W, H]): string =
  result = ""
  for y in mat:
    for x in y:
      result.add($x & ", ")
    result.add("\n")

proc `+`[W, H](a, b: Matrix[W, H]): Matrix[W, H] =
  for y in 1..high(a):
    for x in 1..high(a[0]):
      result[y][x] = a[y][x] + b[y][x]

let
  zippedNames = zip(names1, names2)
  summedMat = mat1 + mat2

echo summedMat
