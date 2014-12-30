def is_divisible_by(numerator, denominator):
    return numerator % denominator == 0

def is_leap_year(year):
    if is_divisible_by(year, 400):
        return True
    elif is_divisible_by(year, 100):
        return False
    elif is_divisible_by(year, 4):
        return True
    else:
        return False
