pub mod FpInFlix.Exercises.Parsing.Parsers {
    pub trait Parsers[parser: Type -> Type] {
        pub def string(s: String): parser[String]

        pub def char(c: Char): parser[Char] =
            string("${c}") |> map(String.charAt(0))

        pub def succeed(a: a): parser[a]

        pub def fail(msg: String): parser[a]

        pub def regex(r: Regex): parser[String]

        pub def whitespace(): parser[String] =
            regex(regex"\\s*")

        pub def digits(): parser[String] =
            regex(regex"\\d+")

        pub def thru(s: String): parser[String] =
            regex(".*?${Regex.quote(s)}"
                |> String.toRegex
                |> Result.getWithDefault(Regex.unmatchable()))

        pub def quoted(): parser[String] =
            string("\"") *> lazy thru("\"") |> map(String.dropRight(1))

        pub def escapedQuoted(): parser[String] =
            quoted() |> label("string literal") |> token

        pub def doubleString(): parser[String] =
            regex(regex"[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?") |> token

        pub def double(): parser[Float64] =
            doubleString()
                |> flatMap(
                    Float64.fromString
                        >> Option.foldLeft((_, d) -> succeed(d), fail("not a valid double literal"))
                )
                |> label("double literal")

        pub def eof(): parser[String] =
            regex(regex"\\z") |> label("unexpected trailing characters")

        pub def runParser(input: String, p: parser[a]): Result[ParseError, a]

        pub def orElse(p1: parser[a], p2: Lazy[parser[a]]): parser[a]

        pub def ||(p1: parser[a], p2: Lazy[parser[a]]): parser[a] =
            orElse(p1, p2)

        pub def attempt(p: parser[a]): parser[a]

        pub def listOfN(n: Int32, p: parser[a]): parser[List[a]] =
            if (n <= 0) succeed(Nil)
            else map2(List.cons, p, lazy listOfN(n - 1, p))

        pub def map(f: a -> b, p: parser[a]): parser[b] =
            flatMap(f >> succeed, p)

        pub def map2(f: (a, b) -> c, p1: parser[a], p2: Lazy[parser[b]]): parser[c] =
            product(p1, p2) |> map(t -> t ||> f)

        pub def many(p: parser[a]): parser[List[a]] =
            map2(List.cons, p, lazy many(p)) || lazy succeed(Nil)

        pub def many1(p: parser[a]): parser[List[a]] =
            map2(List.cons, p, lazy many(p))

        pub def slice(p: parser[a]): parser[String]

        pub def opt(p: parser[a]): parser[Option[a]] =
            map(Some, p) || lazy succeed(None)

        pub def product(p1: parser[a], p2: Lazy[parser[b]]): parser[(a, b)] =
            flatMap(a -> map(b -> (a, b), force p2), p1)

        pub def **(p1: parser[a], p2: Lazy[parser[b]]): parser[(a, b)] =
            product(p1, p2)

        pub def flatMap(f: a -> parser[b], p: parser[a]): parser[b]

        pub def label(msg: String, p: parser[a]): parser[a]

        pub def scope(msg: String, p: parser[a]): parser[a]

        pub def *>(p1: parser[a], p2: Lazy[parser[b]]): parser[b] =
            map2((_, b) -> b, slice(p1), p2)

        pub def <*(p1: parser[a], p2: Lazy[parser[b]]): parser[a] =
            map2((a, _) -> a, p1, lazy slice(force p2))

        pub def token(p: parser[a]): parser[a] =
            attempt(p) <* lazy whitespace()

        pub def sep(separator: parser[a], p: parser[b]): parser[List[b]] =
            sep1(separator, p) || lazy succeed(Nil)

        pub def sep1(separator: parser[a], p: parser[b]): parser[List[b]] =
            map2(List.cons, p, lazy many(separator *> lazy p))

        pub def to(a: a, p: parser[b]): parser[a] =
            map(_ -> a, slice(p))

        pub def opL(op: parser[(a, a) -> a], p: parser[a]): parser[a] =
            map2((a, ts) ->
                List.foldLeft((acc, t) ->
                    let (f, a2) = t;
                    f(acc, a2), a, ts
                ),
                p,
                lazy many(op ** lazy p)
            )

        pub def root(p: parser[a]): parser[a] =
            p <* lazy eof()
    }
}
