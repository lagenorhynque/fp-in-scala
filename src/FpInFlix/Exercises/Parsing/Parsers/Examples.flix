pub mod FpInFlix.Exercises.Parsing.Parsers.Examples {
    use FpInFlix.Exercises.Parsing.Parsers.{Parsers => P}

    pub def nonNegativeInt(): parser[Int32] with P[parser] =
        P.flatMap(
            Int32.fromString
                >> Option.foldLeft(
                    (_, n) -> P.succeed(n),
                    P.fail("expected an integer")
                ),
            P.regex(regex"[0-9]+")
        )

    pub def nConsecutiveAs(): parser[Int32] with P[parser] =
        P.flatMap(
            n ->
                P.map(
                    _ ->
                        n,
                    P.listOfN(n, P.char('a'))
                ),
            nonNegativeInt()
        )
}
