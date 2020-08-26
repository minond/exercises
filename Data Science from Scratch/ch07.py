from __future__ import division

from collections import Counter

import sys
import math
import random

import numpy as np

from numpy.random import seed
from numpy.random import randint

from matplotlib import pyplot as plt

from ch06 import normal_cdf


def normal_approximation_to_binomial(n, p):
    mu = p * n
    sigma = math.sqrt(p * (1 - p) * n)
    return mu, sigma


def normal_probability_below(*args, **kwargs):
    return normal_cdf(*args, **kwargs)


def normal_probability_above(lo, mu=0, sigma=1):
    return 1 - normal_cdf(lo, mu, sigma)


def normal_probability_between(lo, hi, mu=0, sigma=1):
    return normal_cdf(hi, mu, sigma) - normal_cdf(lo, mu, sigma)


def normal_probability_outside(lo, hi, mu=0, sigma=1):
    return 1 - normal_probability_between(lo, hi, mu, sigma)


def normal_upper_bound(probability, mu=0, sigma=1):
    return inverse_normal_cdf(probability, mu, sigma)


def normal_lower_bound(probability, mu=0, sigma=1):
    return inverse_normal_cdf(1 - probability, mu, sigma)


def inverse_normal_cdf(p, mu=0, sigma=1, tolerance=0.00001):
    if mu != 0 or sigma != 1:
        return mu + sigma * inverse_normal_cdf(p, tolerance=tolerance)

    low_z = -10.0
    hi_z  =  10.0

    while hi_z - low_z > tolerance:
        mid_z = (low_z + hi_z) / 2
        mid_p = normal_cdf(mid_z)

        if mid_p < p:
            low_z = mid_z
        elif mid_p > p:
            hi_z = mid_z
        else:
            break

    return mid_z


def two_sided_p_value(x, mu=0, sigma=1):
    if x >= mu:
        return 2 * normal_probability_above(x, mu, sigma)
    else:
        return 2 * normal_probability_below(x, mu, sigma)


def normal_two_sided_bound(probability, mu=0, sigma=1):
    tail_probability = (1 - probability) / 2
    upper_bound = normal_lower_bound(tail_probability, mu, sigma)
    lower_bound = normal_upper_bound(tail_probability, mu, sigma)
    return lower_bound, upper_bound


def ex_1():
    mu_0, sigma_0 = normal_approximation_to_binomial(1000, 0.5)
    print((mu_0, sigma_0))

    lo, hi = normal_two_sided_bound(0.95, mu_0, sigma_0)
    print((lo, hi))

    mu_1, sigma_1 = normal_approximation_to_binomial(1000, 0.55)
    print((mu_1, sigma_1))

    type_2_probability = normal_probability_between(lo, hi, mu_1, sigma_1)
    power = 1 - type_2_probability
    print(power)

    hi = normal_upper_bound(0.95, mu_0, sigma_0)
    type_2_probability = normal_probability_below(hi, mu_1, sigma_1)
    power = 1 - type_2_probability
    print((hi, power))

    print(two_sided_p_value(529.5, mu_0, sigma_0))

    # extreme_value_count = 0
    # for _ in range(100000):
    #     num_heads = sum(1 if random.random() < 0.5 else 0
    #                     for _ in range(1000))
    #     if num_heads >= 530 or num_heads <= 470:
    #         extreme_value_count += 1
    #
    # print(extreme_value_count / 100000)

    print(two_sided_p_value(531.5, mu_0, sigma_0))

    upper_p_value = normal_probability_above
    lower_p_value = normal_probability_below

    print(upper_p_value(524.5, mu_0, sigma_0))


def call(name):
    print("calling %s" % name)
    getattr(sys.modules[__name__], name)()


if __name__ == "__main__":
    seed(434353432)

    if len(sys.argv) > 1:
        call("ex_%s" % sys.argv[1])
    else:
        exs = [int(ex.replace("ex_", "")) for ex in dir(sys.modules[__name__]) if ex.startswith("ex_")]
        exs.sort()
        call("ex_%s" % exs[-1])
