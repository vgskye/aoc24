from collections import defaultdict

def BronKerbosch(N, R, P, X):
    if len(P) == 0 and len(X) == 0:
        return [R]
    ret = []
    for v in min([P - N[u] for u in P | X], key=len):
        ret += BronKerbosch(N, R | {v}, P & N[v], X & N[v])
        P -= {v}
        X |= {v}
    return ret

with open("day23.txt") as f:
    pairs = [a.strip().split("-") for a in f.readlines()]
    N = defaultdict(set)
    for pair in pairs:
        N[pair[0]].add(pair[1])
        N[pair[1]].add(pair[0])
    maximals = BronKerbosch(N, set(), N.keys(), set())
    print(",".join(sorted(max(maximals, key=len))))