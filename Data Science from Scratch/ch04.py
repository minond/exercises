from __future__ import division

import sys
import math

from numpy.random import seed
from numpy.random import randint


def vector_add(a, b):
    return [a_i + b_i for a_i, b_i in zip(a, b)]


def vector_subtract(a, b):
    return [a_i - b_i for a_i, b_i in zip(a, b)]


def vector_sum(vectors):
    return reduce(vector_add, vectors)


def scalar_multiply(x, vec):
    return [x * i for i in vec]


def vector_mean(vectors):
    n = len(vectors)
    return scalar_multiply(1/n, vector_sum(vectors))


def dot(v, w):
    """v_1 * w_1 + ... + v_n * w_n"""
    return sum(v_i * w_i for v_i, w_i in zip(v, w))


def sum_of_squares(v):
    return dot(v, v)


def magnitude(v):
    return math.sqrt(sum_of_squares(v))


def squared_distance(v, w):
    """(v_1 - w_1)^2 + ... + (v_n - w_n)^2"""
    return sum_of_squares(vector_subtract(v, w))


def distance(v, w):
    return math.sqrt(squared_distance(v, w))


def shape(A):
    rows = len(A)
    cols = len(A[0]) if A else 0
    return rows, cols


def get_row(A, i):
    return A[i]


def get_column(A, i):
    return [row[i] for row in A]


def make_matrix(rows, cols, gen):
    return [[gen(r, c) for c in range(cols)]
                for r in range(rows)]


def is_diagonal(i, j):
    return 1 if i == j else 0


def ex_14():
    identify_matrix = make_matrix(5, 5, is_diagonal)
    print(identify_matrix)


def ex_13():
    print(make_matrix(10, 10, lambda a, b: randint(100)))


def ex_12():
    A = [[randint(100) for _ in range(10)] for _ in range(5)]
    print(A)
    print(get_row(A, 2))
    print(get_column(A, 3))


def ex_11():
    A = [[randint(100) for _ in range(10)] for _ in range(5)]
    s = shape(A)
    print(A)
    print(s)


def ex_10():
    a = [randint(100) for _ in range(10)]
    b = [randint(100) for _ in range(10)]
    x = distance(a, b)
    print(a)
    print(b)
    print(x)


def ex_9():
    a = [randint(100) for _ in range(10)]
    b = [randint(100) for _ in range(10)]
    x = squared_distance(a, b)
    print(a)
    print(b)
    print(x)


def ex_8():
    a = [randint(100) for _ in range(10)]
    x = magnitude(a)
    print(a)
    print(x)


def ex_7():
    a = [randint(100) for _ in range(10)]
    x = sum_of_squares(a)
    print(a)
    print(x)


def ex_6():
    a = [randint(100) for _ in range(10)]
    b = [randint(100) for _ in range(10)]
    d = dot(a, b)
    print(a)
    print(b)
    print(d)


def ex_5():
    vectors = [[randint(100) for _ in range(10)] for _ in range(5)]
    result = vector_mean(vectors)
    print(vectors)
    print(result)


def ex_4():
    vector = [randint(100) for _ in range(10)]
    result = scalar_multiply(11, vector)
    print(vector)
    print(result)


def ex_3():
    vectors = [[randint(100) for _ in range(10)] for _ in range(5)]
    s = vector_sum(vectors)
    print(vectors)
    print(s)


def ex_2():
    a = [randint(100) for _ in range(10)]
    b = [randint(100) for _ in range(10)]
    c = vector_subtract(a, b)
    print(a)
    print(b)
    print(c)


def ex_1():
    a = [randint(100) for _ in range(10)]
    b = [randint(100) for _ in range(10)]
    c = vector_add(a, b)
    print(a)
    print(b)
    print(c)


def call(name):
    print("calling %s" % name)
    getattr(sys.modules[__name__], name)()


if __name__ == "__main__":
    seed(43435343274328473)

    if len(sys.argv) > 1:
        call("ex_%s" % sys.argv[1])
    else:
        exs = [int(ex.replace("ex_", "")) for ex in dir(sys.modules[__name__]) if ex.startswith("ex_")]
        exs.sort()
        call("ex_%s" % exs[-1])
