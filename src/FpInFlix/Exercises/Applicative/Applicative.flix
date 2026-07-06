pub mod FpInFlix.Exercises.Applicative.Applicative {
    use FpInFlix.Exercises.Monads.Functor
    use FpInFlix.Exercises.State.State

    pub trait Applicative[f: Type -> Type] with Functor[f] {
        pub def unit(a: a): f[a]
        pub def apply(fab: f[a -> b], fa: f[a]): f[b] =
            map2((f, a) -> f(a), fab, fa)

        pub def map2(f: (a, b) -> c, fa: f[a], fb: f[b]): f[c] =
            apply(apply(unit(f), fa), fb)
        pub def map(f: a -> b, fa: f[a]): f[b] =
            apply(unit(f), fa)
        pub def sequence(fas: List[f[a]]): f[List[a]] =
            traverse(identity, fas)
        pub def traverse(f: a -> f[b], xs: List[a]): f[List[b]] =
            List.foldRight((x, acc) -> map2(List.cons, f(x), acc), unit(Nil), xs)
        pub def replicateM(n: Int32, fa: f[a]): f[List[a]] =
            sequence(List.repeat(n, fa))
        pub def product(fa: f[a], fb: f[b]): f[(a, b)] =
            map2((a, b) -> (a, b), fa, fb)
    }

    mod ZipList {
        use FpInFlix.Exercises.Applicative.Applicative.ZipList.toDelayList
        use FpInFlix.Exercises.Applicative.Applicative
        use FpInFlix.Exercises.Monads.Functor

        pub enum ZipList[a](DelayList[a]) with Eq, ToString

        pub def fromDelayList(dl: DelayList[a]): ZipList[a] =
            ZipList(dl)
        pub def toDelayList(zl: ZipList[a]): DelayList[a] =
            let ZipList(dl) = zl;
            dl

        instance Functor[ZipList] {
            pub def map(f: a -> b, fa: ZipList[a]): ZipList[b] =
                fa |> toDelayList |> DelayList.map(f) |> fromDelayList
        }

        instance Applicative[ZipList] {
            pub def unit(a: a): ZipList[a] =
                DelayList.repeat(a) |> fromDelayList
            pub redef map2(f: (a, b) -> c, fa: ZipList[a], fb: ZipList[b]): ZipList[c] =
                DelayList.zip(toDelayList(fa), toDelayList(fb))
                    |> DelayList.map(t -> let (a, b) = t; f(a, b))
                    |> fromDelayList
        }
    }

    mod Semigroup {
        pub trait Semigroup[a] {
            pub def combine(a1: a, a2: a): a
        }
    }

    mod Monoid {
        use FpInFlix.Exercises.Applicative.Applicative.Semigroup

        pub trait Monoid[a] with Semigroup[a] {
            pub def empty(): a
        }
    }

    mod NonEmptyList {
        use FpInFlix.Exercises.Applicative.Applicative.NonEmptyList
        use FpInFlix.Exercises.Applicative.Applicative.Semigroup

        pub enum NonEmptyList[a](a, List[a]) with Eq, ToString

        pub def toList(xs: NonEmptyList[a]): List[a] =
            let NonEmptyList(head, tail) = xs;
            head :: tail

        instance Semigroup[NonEmptyList[a]] {
            pub def combine(a1: NonEmptyList[a], a2: NonEmptyList[a]): NonEmptyList[a] =
                let NonEmptyList(head1, tail1) = a1;
                let NonEmptyList(head2, tail2) = a2;
                NonEmptyList(head1, tail1 ++ (head2 :: tail2))
        }
    }

    mod Const {
        use FpInFlix.Exercises.Applicative.Applicative
        use FpInFlix.Exercises.Applicative.Applicative.{Monoid, Semigroup}
        use FpInFlix.Exercises.Monads.Functor

        pub enum Const[a, _b](a)

        instance Functor[Const[m]] with Monoid[m] {
            pub def map(f: a -> b, fa: Const[m, a]): Const[m, b] =
                Applicative.apply(Applicative.unit(f), fa)
        }

        instance Applicative[Const[m]] with Monoid[m] {
            pub def unit(_: a): Const[m, a] =
                Const(Monoid.empty())
            pub redef apply(fab: Const[m, a -> b], fa: Const[m, a]): Const[m, b] =
                let Const(m1) = fab;
                let Const(m2) = fa;
                Const(Semigroup.combine(m1, m2))
        }
    }

    instance Applicative[State[s]] {
        pub def unit(a: a): State[s, a] =
            State.unit(a)
        pub redef apply(fab: State[s, a -> b], fa: State[s, a]): State[s, b] =
            State.map2((f, a) -> f(a), fab, fa)
    }
}
