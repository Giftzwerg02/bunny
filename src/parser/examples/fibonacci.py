def fib(n):
    acc = [0, 1]
    for i in range(n):
        l = len(acc)
        f1 = acc[l - 1]
        f2 = acc[l - 2]
        res = f1 + f2
        zero = i - i
        resa = zero + res
        acc.append(resa)
    return acc

res = fib(10000)
print("done", res[:-1][0])
