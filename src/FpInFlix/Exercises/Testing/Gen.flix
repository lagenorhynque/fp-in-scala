pub mod FpInFlix.Exercises.Testing.Gen {
    use FpInFlix.Exercises.State.{Rng, State}

    pub enum Gen[a](State[Rng, a])

    pub def map(f: a -> b, ga: Gen[a]): Gen[b] =
        let Gen(sa) = ga;
        State.map(f, sa) |> Gen

    pub def map2(f: (a, b) -> c, ga: Gen[a], gb: Gen[b]): Gen[c] =
        let Gen(sa) = ga;
        let Gen(sb) = gb;
        State.map2(f, sa, sb) |> Gen

    pub def flatMap(f: a -> Gen[b], ga: Gen[a]): Gen[b] =
        let Gen(sa) = ga;
        State.flatMap(a -> let Gen(sb) = f(a); sb, sa) |> Gen

    pub def listOf(size: Gen[Int32], ga: Gen[a]): Gen[List[a]] =
        Gen.flatMap(n -> listOfN(n, ga), size)

    pub def list(ga: Gen[a]): SGen[List[a]] =
        SGen(n -> listOfN(n, ga))

    pub def nonEmptyList(ga: Gen[a]): SGen[List[a]] =
        SGen(n -> listOfN(Int32.max(n, 1), ga))

    pub def unsized(ga: Gen[a]): SGen[a] =
        SGen(_ -> ga)

    pub def **(ga: Gen[a], gb: Gen[b]): Gen[(a, b)] =
        map2((a, b) -> (a, b), ga, gb)

    pub def unit(a: a): Gen[a] =
        a |> State.unit |> Gen

    pub def boolean(): Gen[Bool] =
        Rng.boolean() |> State |> Gen

    pub def chooseInt(start: Int32, stopExclusive: Int32): Gen[Int32] =
        Rng.nonNegativeInt()
            |> State
            |> State.map(n -> start + n `Int32.remainder` (stopExclusive - start))
            |> Gen

    pub def listOfN(size: Int32, ga: Gen[a]): Gen[List[a]] =
        let Gen(sa) = ga;
        List.repeat(size, sa) |> State.sequence |> Gen

    pub def double(): Gen[Float64] =
        Rng.double() |> State |> Gen

    pub def int(): Gen[Int32] =
        Rng.int() |> State |> Gen

    pub def chooseDouble(i: Float64, j: Float64): Gen[Float64] =
        Rng.double()
            |> State
            |> State.map(d -> i + d * (j - i))
            |> Gen

    pub def union(g1: Gen[a], g2: Gen[a]): Gen[a] =
        flatMap(b -> if (b) g1 else g2, boolean())

    pub def smallInt(): Gen[Int32] =
        chooseInt(-10, 10)

}
