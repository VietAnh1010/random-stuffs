from __future__ import annotations
from dataclasses import dataclass
from math import inf

@dataclass(slots=True, frozen=True)
class Point:
    x: int | float
    y: int | float

    def __sub__(self, other: Point):
        return Point(self.x - other.x, self.y - other.y)
    
    def dot(self, other: Point):
        return self.x * other.x + self.y * other.y
    
    def cross(self, other: Point):
        return self.x * other.y - self.y * other.x
    
    def dotw(self, other1: Point, other2: Point):
        return (other1 - self).dot(other2 - self)
    
    def crossw(self, other1: Point, other2: Point):
        return (other1 - self).cross(other2 - self)

    @property
    def norm_sqr(self):
        return self.dot(self)
    
inf_vec = Point(inf, inf)

@dataclass(init=False, slots=True)
class QuadEdge:
    org: Point
    rot: QuadEdge
    onext: QuadEdge
    used: bool = False

    def __init__(self, org):
        self.org = org
        self.rot = self
        self.onext = self
        self.used = False

    @property
    def sym(self):
        return self.rot.rot
    
    @property
    def tor(self):
        return self.sym.rot
    
    @property
    def lnext(self):
        return self.tor.onext.rot
    
    @property
    def rprev(self):
        return self.sym.onext
    
    @property
    def oprev(self):
        return self.rot.onext.rot

    @property
    def dest(self):
        return self.sym.org
    
def make_edge(org: Point, dest: Point):
     # makes 4 quad edges
    e1 = QuadEdge(org)     # one from the origion
    e2 = QuadEdge(dest)    # one from the destination
    e3 = QuadEdge(inf_vec) # and 2 goes from infinity?
    e4 = QuadEdge(inf_vec)
    e1.rot = e3
    e2.rot = e4
    e3.rot = e2
    e4.rot = e1
    e1.onext = e1
    e2.onext = e2
    e3.onext = e4
    e4.onext = e3
    return e1

def splice(a: QuadEdge, b: QuadEdge):
    a.onext.rot.onext, b.onext.rot.onext = b.onext.rot.onext, a.onext.rot.onext
    a.onext, b.onext = b.onext, a.onext

def delete_edge(e: QuadEdge):
    splice(e, e.oprev)
    splice(e.sym, e.sym.oprev)

def connect(a: QuadEdge, b: QuadEdge):
    e = make_edge(a.dest, b.org)
    splice(e, a.lnext)
    splice(e.sym, b)
    return e

def left_of(p: Point, e: QuadEdge):
    return p.crossw(e.org, e.dest) > 0

def right_of(p: Point, e: QuadEdge):
    return p.crossw(e.org, e.dest) < 0

def det3(a1, a2, a3, b1, b2, b3, c1, c2, c3):
    return a1 * (b2 * c3 - c2 * b3) - a2 * (b1 * c3 - c1 * b3) + a3 * (b1 * c2 - c1 * b2)

def in_circle(a: Point, b: Point, c: Point, d: Point):
    det = -det3(b.x, b.y, b.norm_sqr, c.x, c.y, c.norm_sqr, d.x, d.y, d.norm_sqr)
    det += det3(a.x, a.y, a.norm_sqr, c.x, c.y, c.norm_sqr, d.x, d.y, d.norm_sqr)
    det -= det3(a.x, a.y, a.norm_sqr, b.x, b.y, b.norm_sqr, d.x, d.y, d.norm_sqr)
    det += det3(a.x, a.y, a.norm_sqr, b.x, b.y, b.norm_sqr, c.x, c.y, c.norm_sqr)
    return det > 0


def build_tr(l: int, r: int, ps: list[Point]) -> tuple[QuadEdge, QuadEdge]:
    if r - l == 1:
        res = make_edge(ps[l], ps[r])
        return res, res.sym
    if r - l == 2:
        a = make_edge(ps[l], ps[l + 1])
        b = make_edge(ps[l + 1], ps[r])
        splice(a.sym, b)
        # if the points are co-linear
        sg = ps[l].crossw(ps[l + 1], ps[r])
        if sg == 0:
            # we just need to return these 2
            return a, b.sym
        c = connect(b, a)
        if sg > 0:
            return a, b.sym
        return c.sym, c
    # merge 2 triagular together
    mid = (l + r) // 2
    # recursively solve the 2 halves
    ldo, ldi = build_tr(l, mid, ps)
    rdi, rdo = build_tr(mid + 1, r, ps)
    while True:
        if left_of(rdi.org, ldi):
            ldi = ldi.lnext
            continue
        if right_of(ldi.org, rdi):
            rdi = rdi.rprev
            continue
        break
    
    basel = connect(rdi.sym, ldi)
    if ldi.org == ldo.org:
        ldo = basel.sym
    if rdi.org == rdo.org:
        rdo = basel
    
    def valid(e: QuadEdge):
        return right_of(e.dest, basel)
    
    while True:
        lcand = basel.sym.onext
        if valid(lcand):
            while in_circle(basel.dest, basel.org, lcand.dest, lcand.onext.dest):
                t = lcand.onext
                delete_edge(lcand)
                lcand = t
        rcand = basel.oprev
        if valid(rcand):
            while in_circle(basel.dest, basel.org, rcand.dest, rcand.oprev.dest):
                t = rcand.oprev
                delete_edge(rcand)
                rcand = t
        if not valid(lcand) and not valid(rcand):
            break
        if not valid(lcand) or (valid(rcand) and in_circle(lcand.dest, lcand.org, rcand.org, rcand.dest)):
            basel = connect(rcand, basel.sym)
        else:
            basel = connect(basel.sym, lcand.sym)
    return ldo, rdo

def deulaunay(ps: list[Point]):
    print("start running...")
    ps.sort(key=lambda p: (p.x, p.y))
    res = build_tr(0, len(ps) - 1, ps)
    e = res[0]
    edges = [e]
    while e.onext.dest.crossw(e.dest, e.org) < 0:
        e = e.onext
    def add():
        curr = e
        while True:
            curr.used = True
            ps.append(curr.org)
            edges.append(curr.sym)
            curr = curr.lnext
            if curr == e:
                break
    add()
    ps.clear()
    kek = 0
    while kek < len(edges):
        e = edges[kek]
        if not e.used:
            add()
        kek += 1
    print(f"{ps=}")
    ans = []
    for i in range(0, len(ps), 3):
        # construct all trianges in the Delaunay triagulation of this graph
        # for euclid minimum spanning tree, this is not necessary
        ans.append((ps[i], ps[i + 1], ps[i + 2]))
    print("finish!")
    return ans

def main():
    points = [
        Point(0, 0),
        Point(1, 1),
        Point(1, 2),
        Point(2, 3),
        Point(3, 4)
    ]
    print(deulaunay(points))

if __name__ == "__main__":
    main()