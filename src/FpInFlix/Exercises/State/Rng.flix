pub mod FpInFlix.Exercises.State.Rng {
   pub enum Rng(Int64) with Eq, ToString

   pub def nextInt(rng: Rng): (Int32, Rng) =
       let Rng(seed) = rng;
       let newSeed = Int64.bitwiseAnd(seed * 25214903917i64 + 11i64, 281474976710655i64);
       let nextRng = Rng(newSeed);
       let n = Int64.rightShift(newSeed, 16) |> Int64.truncateToInt32;
       (n, nextRng)

    pub type alias Rand[a] = Rng -> (a, Rng)

    pub def int(): Rand[Int32] =
        rng -> nextInt(rng)

    pub def unit(a: a): Rand[a] =
        rng -> (a, rng)

    pub def map(f: a -> b, ra: Rand[a]): Rand[b] =
        rng ->
            let (a, rng2) = ra(rng);
            (f(a), rng2)

    pub def nonNegativeInt(): Rand[Int32] =
        rng ->
            let (i, rng2) = nextInt(rng);
            (if (i < 0) -(i + 1) else i, rng2)

    pub def double(): Rand[Float64] =
        rng ->
            let (i, rng2) = nonNegativeInt()(rng);
            (Int32.toFloat64(i) / (Int32.toFloat64(Int32.maxValue()) + 1.0), rng2)

    pub def boolean(): Rand[Bool] =
        rng ->
            let (i, rng2) = nextInt(rng);
            (i `Int32.remainder` 2 == 0, rng2)

    pub def map2(f: (a, b) -> c, ra: Rand[a], rb: Rand[b]): Rand[c] =
        rng0 ->
            let (a, rng1) = ra(rng0);
            let (b, rng2) = rb(rng1);
            (f(a, b), rng2)

    pub def sequence(rs: List[Rand[a]]): Rand[List[a]] =
        List.foldRight((r, acc) -> map2(List.cons, r, acc), unit(Nil), rs)

    pub def flatMap(f: a -> Rand[b], ra: Rand[a]): Rand[b] =
        rng0 ->
            let (a, rng1) = ra(rng0);
            f(a, rng1)

    pub def nonNegativeLessThan(n: Int32): Rand[Int32] =
        flatMap(
            i ->
                let modulo = i `Int32.remainder` n;
                if (i + (n - 1) - modulo >= 0) unit(modulo) else nonNegativeLessThan(n),
            nonNegativeInt()
        )

    pub def mapViaFlatMap(f: a -> b, ra: Rand[a]): Rand[b] =
        flatMap(f >> unit, ra)

    pub def map2ViaFlatMap(f: (a, b) -> c, ra: Rand[a], rb: Rand[b]): Rand[c] =
        flatMap(
            a ->
                map(
                    b -> f(a, b),
                    rb
                ),
            ra
        )
}
