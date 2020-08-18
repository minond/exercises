import sys

from collections import Counter

from numpy.random import seed
from numpy.random import randint
from numpy.random import randn

from matplotlib import pyplot as plt


def ex_5():
    friends = [randint(20, 100) for _ in range(10)]
    minutes = [randint(100, 200) for _ in range(10)]
    labels  = [chr(ord("a") + x) for x in range(10)]

    plt.scatter(friends, minutes)

    for label, friend_count, minute_count in zip(labels, friends, minutes):
        plt.annotate(label,
                     xy=(friend_count, minute_count),
                     xytext=(5, -5),
                     textcoords="offset points")

    plt.axis("equal")
    plt.title("Daily Minutes vs. Number of Friends")
    plt.xlabel("# of Friends")
    plt.ylabel("Daily minutes spends on site")
    plt.show()


def ex_4():
    variance = [1, 2, 4, 8, 16, 32, 64, 128, 256]
    bias_squared = [256, 128, 64, 32, 16, 8, 4, 2, 1]
    total_error = [x + y for x, y in zip(variance, bias_squared)]
    xs = [i for i, _ in enumerate(variance)]

    plt.plot(xs, variance, "g-", label="variance")
    plt.plot(xs, bias_squared, "r-", label="bias^2")
    plt.plot(xs, total_error, "b:", label="total err")

    plt.legend(loc=9)
    plt.xlabel("model complexity")
    plt.title("The Bias-Variace Tradeoff")

    plt.show()


def ex_3():
    grades = [83, 04, 01, 87, 70, 0, 85, 52, 100, 67, 73, 77, 0]
    decile = lambda grade: grade // 10 * 10
    histogram = Counter(decile(grade) for grade in grades)

    plt.bar([x - 4 for x in histogram.keys()],  # Shift each bar to the left by 4
            histogram.values(),                 # Give each bar its correct length
            8)                                  # Give each bar a width of 8

    plt.axis([-5, 105, 0, 5])
    plt.xticks([10 * i for i in range(11)])
    plt.xlabel("Decile")
    plt.ylabel("# of Students")
    plt.title("Distribution of Exam 1 Grades")
    plt.show()


def ex_2():
    movies = ["Annie Hall", "Ben-Hur", "Casablanca", "Gandhi", "West Side Story"]
    num_oscars = [5, 11, 3, 8, 10]

    # Bar have a width of 0.8 by default, so we'll add 0.1 to the left coordinates
    # so that each bar is centered.
    xs = [i + 0.1 for i, _ in enumerate(movies)]

    # Plot bars with left x-coordinates of `xs` and heights of `num_oscars`.
    plt.bar(xs, num_oscars)

    plt.ylabel("# of Academy Awards")
    plt.title("My Favorite Movies")

    # Label x-axis with movie names at bar centers
    plt.xticks([i + 0.5 for i, _ in enumerate(movies)], movies)

    plt.show()


def ex_1():
    years = range(1950, 2020, 10)
    gdp = [244.4, 343.6, 654.4, 756.3, 945.6, 1234.5, 1542.55]

    # Create a line chart
    plt.plot(years, gdp,
             color="green",
             marker="o",
             linestyle="solid")

    # Set the title
    plt.title("Nominal GDP")

    plt.ylabel("Billions of $")
    plt.show()


def call(name):
    print("calling %s" % name)
    getattr(sys.modules[__name__], name)()


if __name__ == "__main__":
    seed(43435343274328473)

    if len(sys.argv) > 1:
        call("ex_%s" % sys.argv[1])
    else:
        exs = [ex for ex in dir(sys.modules[__name__]) if ex.startswith("ex_")]
        exs.sort()
        call(exs[-1])
