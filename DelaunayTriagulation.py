from __future__ import annotations
from dataclasses import dataclass
from math import inf

@dataclass(slots=True, forzen=True)
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

@dataclass(slots=True)
class QuadEdge:
    origin: Point
    rot: QuadEdge | None = None
    onext: QuadEdge | None = None
    used: bool = False

    def rev(self):
        return self.rot.rot
    
    def lnext(self):
        return self.rot.rev().onext.rot
    
    def oprev(self):
        return self.rot.onext.rot

    @property
    def dest(self):
        return self.rev().origin
    
def make_edge(origin: Point, dest: Point):
    e1 = QuadEdge(origin)
    e2 = QuadEdge(dest)
    e3 = QuadEdge(inf_vec)
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
    splice(e, e.oprev())
    splice(e.rev(), e.rev().oprev())

def connect(a: QuadEdge, b: QuadEdge):
    e = make_edge(a.dest, b.origin)
    splice(e, a.lnext())
    splice(e.rev(), b)
    return e

def left_of(p: Point, e: QuadEdge):
    return p.crossw(e.origin, e.dest) > 0

def right_of(p: Point, e: QuadEdge):
    return p.crossw(e.origin, e.dest) < 0

def det3(a1, a2, a3, b1, b2, b3, c1, c2, c3):
    return (a1 * (b2 * c3 - c2 * b3)
        - a2 * (b1 * c3 - c1 * b3) 
        + a3 * (b1 * c2 - c1 * b2))

def in_circle(a: Point, b: Point, c: Point, d: Point):
    det = -det3(b.x, b.y, b.norm_sqr, c.x, c.y, c.norm_sqr, d.x, d.y, d.norm_sqr)
    det += det3(a.x, a.y, a.norm_sqr, c.x, c.y, c.norm_sqr, d.x, d.y, d.norm_sqr)
    det -= det3(a.x, a.y, a.norm_sqr, b.x, b.y, b.norm_sqr, d.x, d.y, d.norm_sqr)
    det += det3(a.x, a.y, a.norm_sqr, b.x, b.y, b.norm_sqr, c.x, c.y, c.norm_sqr)
    return det > 0


def build_tr(l: int, r: int, ps: list[Point]) -> tuple[QuadEdge, QuadEdge]:
    if r - l + 1 == 2:
        res = make_edge(ps[l], ps[r])
        return res, res.rev()
    if r - l + 1 == 3:
        a = make_edge(ps[l], ps[l + 1])
        b = make_edge(ps[l + 1], ps[r])
        sg = ps[l].cross(ps[l + 1], ps[r])
        if sg == 0:
            return a, b.rev()
        if sg > 0:
            return a, b.rev()
        c = connect(b, a)
        return c.rev(), c
    mid = (l + r) // 2
    ldo, ldi = build_tr(l, mid, ps)
    rdi, rdo = build_tr(mid + 1, r, ps)
    while True:
        if left_of(rdi.origin, ldi):
            ldi = ldi.lnext()
            continue
        if right_of(ldi.origin, rdi):
            rdi = rdi.rev().onext
            continue
        break
    basel = connect(rdi.rev(), ldi)
    if ldi.origin == ldo.origin:
        ldo = basel.rev()
    if rdi.origin == rdo.origin:
        rdo = basel
    def valid(e: QuadEdge):
        return right_of(e.dest, basel)
    while True:
        lcand = basel.rev().onext
        if valid(lcand):
            while in_circle(basel.dest, basel.origin, lcand.dest, lcand.onext.dest):
                t = lcand.onext
                delete_edge(lcand)
                lcand = t
        rcand = basel.oprev()
        if valid(rcand):
            while in_circle(basel.dest, basel.origin, rcand.dest, rcand.oprev().dest):
                t = rcand.oprev()
                delete_edge(rcand)
                rcand = t
        if not valid(lcand) and not valid(rcand):
            break
        if not valid(lcand) or (valid(rcand) and in_circle(lcand.dest, lcand.origin, rcand.origin, rcand.dest)):
            basel = connect(rcand, basel.rev())
        else:
            basel = connect(basel.rev(), lcand.rev())
    return ldo, rdo

def deulaunay(ps: list[Point]):
    ps.sort(key=lambda p: (p.x, p.y))
    res = build_tr(0, len(ps) - 1, ps)
    e = res[0]
    edges = [e]
    while e.onext.dest.cross(e.dest, e.origin) < 0:
        e = e.onext
    
    def add():
        curr = e
        while True:
            curr.used = True
            ps.append(curr.origin)
            edges.append(curr.rev())
            curr = curr.lnext()
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
    ans = []
    for i in range(0, len(ps), 3):
        ans.append((ps[i], ps[i + 1], ps[i + 2]))
    return ans