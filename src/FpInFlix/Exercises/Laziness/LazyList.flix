pub mod FpInFlix.Exercises.Laziness.LazyList {
    use FpInFlix.Exercises.Laziness.LazyList.Cons

    pub enum LazyList[a] {
        case Empty
        case Cons(Lazy[a], Lazy[LazyList[a]])
    }

    instance ToString[LazyList[a]] with ToString[a] {
        pub def toString(ll: LazyList[a]): String =
            match ll {
                case Empty => "LazyList.Empty"
                case Cons(_, _) => "LazyList.Cons(<unevaluated>, <unevaluated>)"
            }
    }

    pub def toListRecursive(ll: LazyList[a]): List[a] =
        match ll {
            case Empty => Nil
            case Cons(head, tail) => force head :: toListRecursive(force tail)
        }

    pub def toList(ll: LazyList[a]): List[a] =
        @Tailrec
        def go(xs, acc) =
            match xs {
                case Empty => List.reverse(acc)
                case Cons(head, tail) => go(force tail, force head :: acc)
            };
        go(ll, Nil)

    pub def foldRight(f: (a, Lazy[b]) -> b, z: Lazy[b], ll: LazyList[a]): b =
        match ll {
            case Empty => force z
            case Cons(head, tail) => f(force head, lazy foldRight(f, z, force tail))
        }

    pub def take(n: Int32, ll: LazyList[a]): LazyList[a] =
        match ll {
            case Cons(head, tail) if n > 1 => Cons(head, lazy take(n - 1, force tail))
            case Cons(head, _) if n == 1 => Cons(head, lazy Empty)
            case _ => Empty
        }

    pub def drop(n: Int32, ll: LazyList[a]): LazyList[a] =
        match ll {
            case Cons(_, tail) if n > 0 => drop(n - 1, force tail)
            case _ => ll
        }

    pub def map(f: a -> b, ll: LazyList[a]): LazyList[b] =
        foldRight((x, acc) -> Cons(lazy f(x), acc), lazy Empty, ll)

    pub def ones(): LazyList[Int32] =
        Cons(lazy 1, lazy ones())

    pub def intsFrom(n: Int32): LazyList[Int32] =
        Cons(lazy n, lazy intsFrom(n + 1))

    pub def fibs(): LazyList[BigInt] =
        def go(current, next) =
            Cons(lazy current, lazy go(next, current + next));
        go(0ii, 1ii)
}
