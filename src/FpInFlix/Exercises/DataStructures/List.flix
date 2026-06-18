pub mod FpInFlix.Exercises.DataStructures.List {
    use FpInFlix.Exercises.DataStructures.List.{Cons, Nil}

    pub enum List[a] with Eq, ToString {
        case Nil
        case Cons(a, List[a])
    }

    pub def sum(ints: List[Int32]): Int32 =
        match ints {
            case Nil => 0
            case Cons(x, xs) => x + sum(xs)
        }

    pub def product(doubles: List[Float64]): Float64 =
        match doubles {
            case Nil => 1.0
            case Cons(0.0, _) => 0.0
            case Cons(x, xs) => x * product(xs)
        }

    pub def lengthRec(l: List[a]): Int32 =
        match l {
            case Nil => 0
            case Cons(_, xs) => 1 + lengthRec(xs)
        }

    pub def append(a1: List[a], a2: List[a]): List[a] =
        match a1 {
            case Nil => a2
            case Cons(h, t) => Cons(h, append(t, a2))
        }

    pub def foldRight(f: (a, b) -> b \ ef, acc: b, l: List[a]): b \ ef =
        match l {
            case Nil => acc
            case Cons(x, xs) => f(x, foldRight(f, acc, xs))
        }

    @Tailrec
    pub def foldLeft(f: (b, a) -> b \ ef, acc: b, l: List[a]): b \ ef =
        match l {
            case Nil => acc
            case Cons(x, xs) => foldLeft(f, f(acc, x), xs)
        }

    pub def flatMap(f: a -> List[b] \ ef, l: List[a]): List[b] \ ef =
        foldRight((x, acc) -> append(f(x), acc), Nil, l)

    pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef =
        flatMap(x -> Cons(f(x), Nil), l)

    pub def filter(p: a -> Bool \ ef, l: List[a]): List[a] \ ef =
        flatMap(x -> if (p(x)) Cons(x, Nil) else Nil, l)
}
