import numpy as np

np.seterr(over="ignore")


def fib(n) -> list[np.int64]:
    acc = [np.int64(0), np.int64(1)]
    for i in range(n):
        l = len(acc)
        f1 = acc[l - 1]
        f2 = acc[l - 2]
        res = f1 + f2
        acc.append(res)
    return acc


res = fib(10000)
print(res[-1])
