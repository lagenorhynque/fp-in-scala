pub mod FpInFlix.Exercises.Testing.Gen.SGen {
    use FpInFlix.Exercises.Testing.Gen

    pub enum SGen[a](Int32 -> Gen[a])

    pub def map(f: a -> b, sga: SGen[a]): SGen[b] =
        let SGen(sa) = sga;
        sa >> Gen.map(f) |> SGen

    pub def flatMap(f: a -> SGen[b], sga: SGen[a]): SGen[b] =
        let SGen(sa) = sga;
        SGen(n -> sa(n) |> Gen.flatMap(x -> let SGen(sb) = f(x); sb(n)))

    pub def **(sga: SGen[a], sgb: SGen[b]): SGen[(a, b)] =
        let SGen(sa) = sga;
        let SGen(sb) = sgb;
        SGen(n -> (Gen.**)(sa(n), sb(n)))
}
