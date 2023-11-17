import random

n = 1000
m = 2**31 - 1
values = " ".join(map(str, (random.randint(0, m) for _ in range(n))))

with open("randomValues.txt", "w") as f:
    f.write(values)
