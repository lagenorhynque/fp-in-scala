pub mod FpInFlix.Exercises.Applicative.Monad {
    use FpInFlix.Exercises.Applicative.Applicative
    use FpInFlix.Exercises.State.State

    pub trait Monad[f: Type -> Type] with Applicative[f] {
        pub def flatMap(f: a -> f[b], fa: f[a]): f[b] =
            fa |> map(f) |> join

        pub def compose(f: a -> f[b], g: b -> f[c]): a -> f[c] =
            a -> flatMap(g, f(a))
        pub def join(ffa: f[f[a]]): f[a] =
            flatMap(identity, ffa)
        pub def map(f: a -> b, fa: f[a]): f[b] =
            flatMap(a -> Applicative.unit(f(a)), fa)
        pub def map2(f: (a, b) -> c, fa: f[a], fb: f[b]): f[c] =
            flatMap(a -> map(b -> f(a, b), fb), fa)
    }

    instance Monad[State[s]] {
        pub redef flatMap(f: a -> State[s, b], fa: State[s, a]): State[s, b] =
            State.flatMap(f, fa)
    }
}
