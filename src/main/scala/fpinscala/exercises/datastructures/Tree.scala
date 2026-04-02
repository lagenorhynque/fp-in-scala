package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  // Exercise 3.26
  def depth: Int = this match
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (l.depth max r.depth)

  // Exercise 3.27
  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  // Exercise 3.28

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int =
    fold(_ => 1, _ + 1 + _)

  def depthViaFold: Int =
    fold(_ => 0, (d1, d2) => 1 + (d1 max d2))

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(v => Leaf(f(v)), Branch(_, _))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  // Exercise 3.25
  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(v)      => v
    case Branch(l, r) => l.maximum max r.maximum

  // Exercise 3.28
  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(identity, _ max _)
