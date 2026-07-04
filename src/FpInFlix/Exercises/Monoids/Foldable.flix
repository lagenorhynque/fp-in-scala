pub mod FpInFlix.Exercises.Monoids.Foldable {
    use FpInFlix.Exercises.DataStructures.Tree
    use FpInFlix.Exercises.DataStructures.Tree.{Branch, Leaf}
    use FpInFlix.Exercises.Monoids.Monoid
    use FpInFlix.Exercises.Monoids.Monoid.Endo.Endo

    pub trait Foldable[f: Type -> Type] {
        pub def foldRight(f: (a, b) -> b, acc: b, fa: f[a]): b =
            let Endo(g) = foldMap(a -> Endo(b -> f(a, b)), fa);
            g(acc)
        pub def foldLeft(f: (b, a) -> b, acc: b, fa: f[a]): b =
            let Endo(g) = foldMap(a -> Endo(b -> f(b, a)), fa);
            g(acc)
        pub def foldMap(f: a -> b, fa: f[a]): b with Monoid[b] =
            foldRight((a, b) -> Monoid.combine(f(a), b), Monoid.empty(), fa)

        pub def combineAll(fa: f[a]): a with Monoid[a] =
            foldLeft(Monoid.combine, Monoid.empty(), fa)
        pub def toList(fa: f[a]): List[a] =
            foldRight(List.cons, Nil, fa)
    }

    instance Foldable[List] {
        pub redef foldRight(f: (a, b) -> b, acc: b, fa: List[a]): b =
            List.foldRight(f, acc, fa)
        pub redef foldLeft(f: (b, a) -> b, acc: b, fa: List[a]): b =
            List.foldLeft(f, acc, fa)
        pub redef toList(fa: List[a]): List[a] =
            fa
    }

    instance Foldable[Tree] {
        pub redef foldRight(f: (a, b) -> b, acc: b, fa: Tree[a]): b =
            match fa {
                case Leaf(a) => f(a, acc)
                case Branch(left, right) =>
                    Foldable.foldRight(f, Foldable.foldRight(f, acc, right), left)
            }
        pub redef foldLeft(f: (b, a) -> b, acc: b, fa: Tree[a]): b =
            match fa {
                case Leaf(a) => f(acc, a)
                case Branch(left, right) =>
                    Foldable.foldLeft(f, Foldable.foldLeft(f, acc, left), right)
            }
        pub redef foldMap(f: a -> b, fa: Tree[a]): b with Monoid[b] =
            match fa {
                case Leaf(a) => f(a)
                case Branch(left, right) =>
                    Monoid.combine(Foldable.foldMap(f, left), Foldable.foldMap(f, right))
            }
    }

    instance Foldable[Option] {
        pub redef foldRight(f: (a, b) -> b, acc: b, fa: Option[a]): b =
            match fa {
                case None => acc
                case Some(a) => f(a, acc)
            }
        pub redef foldLeft(f: (b, a) -> b, acc: b, fa: Option[a]): b =
            match fa {
                case None => acc
                case Some(a) => f(acc, a)
            }
        pub redef foldMap(f: a -> b, fa: Option[a]): b with Monoid[b] =
            match fa {
                case None => Monoid.empty()
                case Some(a) => f(a)
            }
    }
}
