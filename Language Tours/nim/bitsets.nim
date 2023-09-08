assert('x' in {'a'..'z'})
assert({'a'..'m'} + {'n'..'z'} == {'a'..'z'})

assert({'a'..'m'} * {'c'..'z'} == {'c'..'m'})
assert({'a'..'c'} <= {'a'..'z'})
