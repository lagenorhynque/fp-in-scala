pub mod FpInFlix.Exercises.Monads.Monad {
    use FpInFlix.Exercises.Monads.Functor
    use FpInFlix.Exercises.State.State

    pub trait Monad[f: Type -> Type] with Functor[f] {
        pub def unit(a: a): f[a]
        pub def flatMap(f: a -> f[b], fa: f[a]): f[b] =
            fa |> map(f) |> join

        pub def compose(f: a -> f[b], g: b -> f[c]): a -> f[c] =
            a -> flatMap(g, f(a))
        pub def join(ffa: f[f[a]]): f[a] =
            flatMap(identity, ffa)
        pub def map(f: a -> b, fa: f[a]): f[b] =
            flatMap(a -> unit(f(a)), fa)
        pub def map2(f: (a, b) -> c, fa: f[a], fb: f[b]): f[c] =
            flatMap(a -> map(b -> f(a, b), fb), fa)
        pub def sequence(fas: List[f[a]]): f[List[a]] =
            List.foldRight((fa, acc) -> map2(List.cons, fa, acc), unit(Nil), fas)
        pub def traverse(f: a -> f[b], xs: List[a]): f[List[b]] =
            List.foldRight((x, acc) -> map2(List.cons, f(x), acc), unit(Nil), xs)
        pub def replicateM(n: Int32, fa: f[a]): f[List[a]] =
            sequence(List.repeat(n, fa))
    }

    instance Monad[State[s]] {
        pub def unit(a: a): State[s, a] =
            State.unit(a)
        pub redef flatMap(f: a -> State[s, b], fa: State[s, a]): State[s, b] =
            State.flatMap(f, fa)
    }

    pub def zipWithIndex(xs: List[a]): List[(Int32, a)] =
        List.foldLeft((acc, x) ->
            Monad.flatMap(ys ->
                Monad.flatMap(n ->
                    Functor.map(_ ->
                        (n, x) :: ys,
                        State.set(n + 1)
                    ),
                    State.get()
                ),
                acc
            ),
            Monad.unit(Nil),
            xs
        )
            |> State.runState(0)
            |> fst
            |> List.reverse
}
