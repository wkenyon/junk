main = f1 g1 g1 g1
f1 f g h = f2 f g h
f2 f g h = f3 f g h
f3 f g h = f4 f g h
f4 f g h = (f . id . id . id) []

g1 = g2
g2 = g3
g3 = g4
g4 = head