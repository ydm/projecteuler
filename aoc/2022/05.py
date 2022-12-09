#!/usr/bin/env python

from itertools import chain


CRATE = '''
[T]     [D]         [L]            
[R]     [S] [G]     [P]         [H]
[G]     [H] [W]     [R] [L]     [P]
[W]     [G] [F] [H] [S] [M]     [L]
[Q]     [V] [B] [J] [H] [N] [R] [N]
[M] [R] [R] [P] [M] [T] [H] [Q] [C]
[F] [F] [Z] [H] [S] [Z] [T] [D] [S]
[P] [H] [P] [Q] [P] [M] [P] [F] [D]
'''

CRATE = '''
    [D]    
[N] [C]    
[Z] [M] [P]
'''
#1   2   3


def crate():
    fi = lambda xs: list(filter(lambda x: x, xs))        # filter by identity
    rp = lambda x: x.replace('[', ' ').replace(']', ' ') # replace parentheses
    rs = lambda x: x.replace(' ', '')

    xs = fi(list(x) for x in rp(CRATE).split('\n'))
    ys = map(''.join, zip(*xs))
    zs = fi(map(str.strip, ys))
    
    return zs


def move(v, q, f, t):
    '''
    Arguments:
    v: crate vector
    q: quantity
    f: from
    t: to
    '''
    v = list(v)                 # make a copy
    print(f"v[f]={v[f]} v[t]={v[t]} q={q} {''.join(reversed(v[f][:q]))}")
    v[t] += ''.join(reversed(v[f][:q]))
    print("after=", v[t])
    v[f] = v[q:]
    return v


def main():
    c = crate()
    with open('05.example', 'r', encoding='ascii') as f:
        for line in map(str.strip, f):
            (_, q, _, f, _, t) = line.split()
            c = move(c, int(q), int(f) - 1, int(t) - 1)
    print(c)


if __name__ == '__main__':
    main()
