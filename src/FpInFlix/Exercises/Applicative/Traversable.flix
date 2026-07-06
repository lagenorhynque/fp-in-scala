pub mod FpInFlix.Exercises.Applicative.Traversable {
    use FpInFlix.Exercises.Applicative.Applicative
    use FpInFlix.Exercises.Applicative.Monad
    use FpInFlix.Exercises.Monads.Functor
    use FpInFlix.Exercises.Monoids.Foldable
    use FpInFlix.Exercises.State.State

    pub trait Traversable[f: Type -> Type] with Functor[f], Foldable[f] {
        pub def traverse(f: a -> g[b], fa: f[a]): g[f[b]] with Applicative[g] =
            fa |> Functor.map(f) |> Traversable.sequence
        pub def sequence(fga: f[g[a]]): g[f[a]] with Applicative[g] =
            Traversable.traverse(identity, fga)

        pub def mapAccum(f: (a, s) -> (b, s), s: s, fa: f[a]): (f[b], s) =
            traverse(a ->
                Monad.flatMap(s1 ->
                    let (b, s2) = f(a, s1);
                    Functor.map(_ ->
                        b,
                        State.set(s2)
                    ),
                    State.get()
                ),
                fa
            ) |> State.runState(s)
    }

    pub def zipWithIndex(fa: f[a]): f[(a, Int32)] with Traversable[f] =
        fa |> Traversable.mapAccum((a, s) -> ((a, s), s + 1), 0) |> fst

    pub def reverse(fa: f[a]): f[a] with Traversable[f] =
        fa
            |> Traversable.mapAccum((_, xs) ->
                (unchecked_cast(Option.getOrAbort(List.head(xs)) as _ \ {}), List.drop(1, xs)),
                fa |> Foldable.toList |> List.reverse
            )
            |> fst

    instance Traversable[List] {
        pub redef traverse(f: a -> g[b], fa: List[a]): g[List[b]] with Applicative[g] =
            List.foldRight((a, acc) ->
                Applicative.map2(List.cons, f(a), acc),
                Applicative.unit(Nil),
                fa
            )
    }

    instance Traversable[Option] {
        pub redef traverse(f: a -> g[b], fa: Option[a]): g[Option[b]] with Applicative[g] =
            Option.foldLeft((_, a) ->
                a |> f |> Functor.map(Some),
                Applicative.unit(None),
                fa
            )
    }

    instance Traversable[Map[k]] with Order[k] {
        pub redef traverse(f: a -> g[b], fa: Map[k, a]): g[Map[k, b]] with Applicative[g] =
            Map.foldLeftWithKey((acc, k, a) ->
                Applicative.map2((m, b) -> Map.insert(k, b, m), acc, f(a)),
                Applicative.unit(Map.empty()),
                fa
            )
    }
}
