from z3 import *

s = Solver()

Z = IntSort()

a, b = Ints('a b')
foo = Function("foo", Z, Z, Z)
bar = Function("bar", Z, Z, Z)
s.add(ForAll([a, b], foo(a, b) == (2 + (a + b))))
s.add(ForAll([a, b], bar(a, b) == ((b + 2) + a)))
print(s.check(ForAll([a, b], foo(a, b) == bar(a, b))))
m = s.model()
print(m.evaluate(foo(a, b)))
print(m.evaluate(bar(a, b)))
