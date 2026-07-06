pub mod FpInFlix.Exercises.Monads.Functor {
    use FpInFlix.Exercises.State.State

    pub trait Functor[f: Type -> Type] {
        pub def map(f: a -> b, fa: f[a]): f[b]
    }

    instance Functor[List] {
        pub def map(f: a -> b, fa: List[a]): List[b] =
            List.map(f, fa)
    }

    instance Functor[State[s]] {
        pub def map(f: a -> b, fa: State[s, a]): State[s, b] =
            State.map(f, fa)
    }
}
