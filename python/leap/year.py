def is_divisible_by(numerator, denominator):
    return numerator % denominator == 0

def is_leap_year(year):
    return is_divisible_by(year, 4) and (not is_divisible_by(year, 100) \
        or is_divisible_by(year, 400))
