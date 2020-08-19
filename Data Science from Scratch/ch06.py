from __future__ import division

from collections import Counter

import sys
import math
import random

import numpy as np

from numpy.random import seed
from numpy.random import randint

from matplotlib import pyplot as plt


def uniform_pdf(x):
    return 1 if x >= 0 and x < 1 else 0


def uniform_cdf(x):
    if x < 0:   return 0
    elif x < 1: return x
    else:       return 1


def normal_pdf(x, mu=0, sigma=1):
    sqrt_two_pi = math.sqrt(math.pi * 2)
    return math.exp(-(x-mu) ** 2 / 2 / sigma ** 2) / (sqrt_two_pi * sigma)


def normal_cdf(x, mu=0, sigma=1):
    return (1 + math.erf((x - mu) / math.sqrt(2) / sigma)) / 2


def benoulli_trial(p):
    return 1 if random.random() < p else 0


def binomail(n, p):
    return sum(benoulli_trial(p) for _ in range(n))


def make_hist(p, n, num_points):
    data = [binomail(n, p) for _ in range(num_points)]

    histogram = Counter(data)
    plt.bar([x - 0.4 for x in histogram.keys()],
            [v / num_points for v in histogram.values()],
            0.8,
            color="0.75")

    mu = p * n
    sigma = math.sqrt(n * p * (1 - p))

    xs = range(min(data), max(data) + 1)
    ys = [normal_cdf(i + 0.5, mu, sigma) - normal_cdf(i - 0.5, mu, sigma)
            for i in xs]

    plt.plot(xs, ys)
    plt.title("Binomial Distribution vs. Normal Approximation")
    plt.show()


def ex_5():
    make_hist(0.75, 100, 10000)


def ex_4():
    xs = [x / 10.0 for x in range(-50, 50)]
    plt.plot(xs, [normal_cdf(x, mu=0, sigma=1) for x in xs], "-", label="mu=0, sigma=1")
    plt.plot(xs, [normal_cdf(x, mu=0, sigma=2) for x in xs], "-", label="mu=0, sigma=2")
    plt.plot(xs, [normal_cdf(x, mu=0, sigma=0.5) for x in xs], "-", label="mu=0, sigma=0.5")
    plt.plot(xs, [normal_cdf(x, mu=-1, sigma=1) for x in xs], "-", label="mu=-1, sigma=1")
    plt.legend()
    plt.title("Variouos Normal cdfs")
    plt.show()


def ex_3():
    xs = [x / 10.0 for x in range(-50, 50)]
    plt.plot(xs, [normal_pdf(x, mu=0, sigma=1) for x in xs], "-", label="mu=0, sigma=1")
    plt.plot(xs, [normal_pdf(x, mu=0, sigma=2) for x in xs], "-", label="mu=0, sigma=2")
    plt.plot(xs, [normal_pdf(x, mu=0, sigma=0.5) for x in xs], "-", label="mu=0, sigma=0.5")
    plt.plot(xs, [normal_pdf(x, mu=-1, sigma=1) for x in xs], "-", label="mu=-1, sigma=1")
    plt.legend()
    plt.title("Variouos Normal pdfs")
    plt.show()


def ex_2():
    for x in np.arange(-1.0, 1.1, 0.1):
        print("x = %s\tuniform_pdf = %s\t uniform_cdf = %s" % (x, uniform_pdf(x), uniform_cdf(x)))


def ex_1():
    def random_kid():
        return random.choice(["boy", "girl"])

    both_girls = 0
    older_girl = 0
    either_girl = 0

    for _ in range(10000):
        younger = random_kid()
        older = random_kid()
        if older == "girl":
            older_girl += 1
        if older == "girl" and younger == "girl":
            both_girls += 1
        if older == "girl" or younger == "girl":
            either_girl += 1

    print("P(both | order) = %s" % (both_girls / older_girl))
    print("P(both | either) = %s" % (both_girls / either_girl))


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
