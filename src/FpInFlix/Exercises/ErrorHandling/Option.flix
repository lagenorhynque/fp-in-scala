pub mod FpInFlix.Exercises.ErrorHandling.Option {
    use FpInFlix.Exercises.ErrorHandling.Option.{None, Some}

    pub enum Option[a] with Eq, ToString {
        case None
        case Some(a)
    }

    pub def map(f: a -> b \ ef, o: Option[a]): Option[b] \ ef =
        match o {
            case None => None
            case Some(x) => Some(f(x))
        }

    pub def getOrElse(default: a, o: Option[a]): a =
        match o {
            case None => default
            case Some(x) => x
        }

    pub def flatMap(f: a -> Option[b] \ ef, o: Option[a]): Option[b] \ ef =
        o |> map(f) |> getOrElse(None)

    pub def flatMap_1(f: a -> Option[b] \ ef, o: Option[a]): Option[b] \ ef =
        match o {
            case None => None
            case Some(x) => f(x)
        }

    pub def filter(p: a -> Bool \ ef, o: Option[a]): Option[a] \ ef =
        match o {
            case None => None
            case Some(x) => if (p(x)) o else None
        }

    pub def filter_1(p: a -> Bool \ ef, o: Option[a]): Option[a] \ ef =
        flatMap(x -> if (p(x)) Some(x) else None, o)

    pub def map2(f: (a, b) -> c \ ef, o1: Option[a], o2: Option[b]): Option[c] \ ef =
        flatMap(x -> map(y -> f(x, y), o2), o1)

    pub def traverse(f: a -> Option[b] \ ef, l: List[a]): Option[List[b]] \ ef =
        match l {
            case Nil => Some(Nil)
            case x :: xs => map2(List.cons, f(x), traverse(f, xs))
        }

    pub def traverse_1(f: a -> Option[b] \ ef, l: List[a]): Option[List[b]] \ ef =
        List.foldRight((x, acc) -> map2(List.cons, f(x), acc), Some(Nil), l)

    pub def sequenceViaTraverse(l: List[Option[a]]): Option[List[a]] =
        traverse(identity, l)
}
