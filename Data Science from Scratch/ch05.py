from __future__ import division

from collections import Counter

import sys
import math

from numpy.random import seed
from numpy.random import randint


# The range is zero when the max and min are equal, which can only happen if
# the elements of x are all the same, which means the data is as undispersed as
# possible. If the range is large, then the max is much larger than the min and
# the data is more spread out.
def data_range(x):
    return max(x) - min(x)


def mean(xs):
    return sum(xs) / len(xs)


def median(xs):
    xs = sorted(xs)

    if len(xs) % 2 == 0:
        l = len(xs)
        a = xs[l // 2]
        b = xs[l // 2 - 1]
        return (a + b) / 2
    else:
        return xs[len(xs) // 2]


def quantile(x, p):
    p_index = int(p * len(x))
    return sorted(x)[p_index]


def mode(x):
    counts = Counter(x)
    max_count = max(counts.values())
    return [x_i for x_i, count in counts.iteritems() if count == max_count]


def dot(v, w):
    """v_1 * w_1 + ... + v_n * w_n"""
    return sum(v_i * w_i for v_i, w_i in zip(v, w))


def sum_of_squares(v):
    return dot(v, v)


def variance(x):
    n = len(x)
    deviations = de_mean(x)
    return sum_of_squares(deviations) / (n - 1)


def de_mean(x):
    x_bar = mean(x)
    return [i - x_bar for i in x]


def standard_deviation(x):
    return math.sqrt(variance(x))


def interquartile_range(x):
    return quantile(x, 0.75) - quantile(x, 0.25)


def covariance(x, y):
    n = len(x)
    return dot(de_mean(x), de_mean(y)) / (n - 1)


def correlation(x, y):
    stdev_x = standard_deviation(x)
    stdev_y = standard_deviation(y)
    if stdev_x > 0 and stdev_y > 0:
        return covariance(x, y) / stdev_x / stdev_y
    else:
        return 0


def ex_6():
    friend_count = [3,  4,  3,  3 , 4,  6,  7,  10,  13,  15,  19]
    minute_count = [30, 50, 30, 40, 40, 50, 70, 100, 140, 140, 200]
    print("friend_count = %s" % friend_count)
    print("minute_count = %s" % minute_count)
    print("correlation = %s" % correlation(friend_count, minute_count))


def ex_5():
    xs = sorted([randint(100) for _ in range(21)])
    print("list = %s" % xs)
    print("quantile(0.25) = %s" % quantile(xs, 0.25))
    print("quantile(0.75) = %s" % quantile(xs, 0.75))
    print("interquartile_range = %s" % interquartile_range(xs))


def ex_4():
    xs = sorted([randint(100) for _ in range(21)])
    print("list = %s" % xs)
    print("mean = %s" % mean(xs))
    print("de_mean = %s" % de_mean(xs))
    print("mean(de_mean) = %s" % mean(de_mean(xs)))
    print("variance = %s" % variance(xs))
    print("standard_deviation = %s" % standard_deviation(xs))


def ex_3():
    xs = sorted([randint(100) for _ in range(21)])
    print("list = %s" % xs)
    print("mode = %s" % mode(xs))


def ex_2():
    x1 = [randint(100) for _ in range(21)]
    x1.sort()

    def print_info(xs):
        print("number = %s" % xs)
        print("min = %s" % xs[0])
        print("max = %s" % xs[-1])
        print("mean = %s" % mean(xs))
        print("median = %s" % median(xs))
        print("quantile(0.10) = %s" % quantile(xs, 0.10))
        print("quantile(0.50) = %s" % quantile(xs, 0.50))
        print("quantile(0.95) = %s" % quantile(xs, 0.95))
        print("quantile(0.99) = %s" % quantile(xs, 0.99))

    print_info(x1)


def ex_1():
    x1 = [randint(100) for _ in range(20)]
    x1.sort()

    x2 = [randint(100) for _ in range(21)]
    x2.sort()

    def print_info(xs):
        print("number = %s" % xs)
        print("min = %s" % xs[0])
        print("max = %s" % xs[-1])
        print("mean = %s" % mean(xs))
        print("median = %s" % median(xs))

    print("Info for x1")
    print_info(x1)
    print("Info for x2")
    print_info(x2)


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
