from functools import lru_cache

# Reverse a string / list


def rev(w):
    return w[::-1]


def is_palindrome(w):
    return w == rev(w)


def is_increasing(w):
    if w == []:
        return True
    t = True
    z = w[0]
    for x in w[1:]:
        t = x >= z
        z = x
        if not t:
            return False
    return t


def sum_symmetric(w):
    n = len(w)
    if n <= 1:
        return True
    else:
        a = w[:int(n/2)]
        b = w[int(n/2):]
        c = [int(i) for i in a]
        d = [int(i) for i in b]
        return sum(c) == sum(d)


@lru_cache(maxsize=None)
def number_of_blocks(w):
    if is_palindrome(w):
        return 1
    else:
        options = [number_of_blocks(w[:i]) +
                   number_of_blocks(w[i:]) for i in range(1, len(w))]
        return min(options)


@lru_cache(maxsize=None)
def blocks(w):
    if is_palindrome(w):
        return (1, [w])
    options = None
    for i in range(1, len(w)):
        nl, wl = blocks(w[:i])
        nr, wr = blocks(w[i:])
        k, ws = nl + nr, wl + wr
        if options is None:
            options = (k, ws)
        else:
            m, _ = options
            if k < m:
                options = (k, ws)
    return options


@lru_cache(maxsize=None)
def number_of_blocks_sym(w, is_symmetric):
    if len(w) == 0:
        return 0
    if is_symmetric(w):
        return 1
    else:
        options = [number_of_blocks_sym(w[:i], is_symmetric) +
                   number_of_blocks_sym(w[i:], is_symmetric)
                   for i in range(1, len(w))]
        return min(options)


@lru_cache(maxsize=None)
def blocks_sym(w, is_symmetric):
    if len(w) == 0:
        return (0, [w])
    if is_symmetric(w):
        return (1, [w])
    options = None
    for i in range(1, len(w)):
        nl, wl = blocks_sym(w[:i], is_symmetric)
        nr, wr = blocks_sym(w[i:], is_symmetric)
        k, ws = nl + nr, wl + wr
        if options is None:
            options = (k, ws)
        else:
            m, _ = options
            if k < m:
                options = (k, ws)
    return options
