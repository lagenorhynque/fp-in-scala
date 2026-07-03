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
            map2(
                (a, ts) -> List.foldLeft((acc, t) ->
                    let (f, a2) = t;
                    f(acc, a2), a, ts),
                p,
                lazy many(op ** lazy p)
            )

        pub def root(p: parser[a]): parser[a] =
            p <* lazy eof()
    }

    mod Location {
        use FpInFlix.Exercises.Parsing.Parsers.ParseError

        pub enum Location({input = String, offset = Int32})

        instance Eq[Location] {
            pub def eq(loc1: Location, loc2: Location): Bool =
                let Location({input = input1, offset = offset1}) = loc1;
                let Location({input = input2, offset = offset2}) = loc2;
                input1 == input2 and offset1 == offset2
        }

        pub def line(loc: Location): Int32 =
            let Location({input, offset}) = loc;
            input |> String.slice(start = 0, end = offset + 1) |> String.countSubstring(substr = "\n") + 1

        pub def col(loc: Location): Int32 =
            let Location({input, offset}) = loc;
            input
                |> String.slice(start = 0, end = offset + 1)
                |> String.findIndexOfRight(c -> c == '\n')
                |> Option.foldLeft((_, lineStart) -> offset - lineStart, offset + 1)

        pub def toError(msg: String, loc: Location): ParseError =
            ParseError(List#{(loc, msg)})

        pub def advanceBy(n: Int32, loc: Location): Location =
            let Location(l) = loc;
            Location({offset = l#offset + n | l})

        pub def remaining(loc: Location): String =
            let Location({input, offset}) = loc;
            String.sliceRight(start = offset, input)

        pub def slice(n: Int32, loc: Location): String =
            let Location({input, offset}) = loc;
            String.slice(start = offset, end = offset + n, input)

        pub def currentLine(loc: Location): String =
            let Location({input | _}) = loc;
            if (String.length(input) > 1)
                input
                    |> String.lines
                    |> List.drop(Location.line(loc) - 1)
                    |> List.head
                    |> Option.getWithDefault("")
            else ""

        pub def columnCaret(loc: Location): String =
            "${String.repeat(Location.col(loc) - 1, " ")}^"
    }

    mod ParseError {
        use FpInFlix.Exercises.Parsing.Parsers.Location

        pub enum ParseError(List[(Location, String)]) with Eq

        instance ToString[ParseError] {
            pub def toString(err: ParseError): String =
                let ParseError(es) = err;
                if (List.isEmpty(es)) "no error message"
                else
                    let collapsed = collapseStack(es);
                    let context =
                        collapsed
                            |> List.last
                            |> Option.map(t -> "\n\n${t |> fst |> Location.currentLine}")
                            |> Option.getWithDefault("")
                        +
                        collapsed
                            |> List.last
                            |> Option.map(t -> "\n${t |> fst |> Location.columnCaret}")
                            |> Option.getWithDefault("");
                    collapsed
                        |> List.map(t ->
                            let (loc, msg) = t;
                            "${formatLoc(loc)} ${msg}"
                        )
                        |> List.join("\n")
                        + context
        }

        pub def push(loc: Location, msg: String, err: ParseError): ParseError =
            let ParseError(es) = err;
            ParseError((loc, msg) :: es)

        pub def label(msg: String, err: ParseError): ParseError =
            err |> latestLoc |> Option.map(l -> (l, msg)) |> Option.toList |> ParseError

        pub def latest(err: ParseError): Option[(Location, String)] =
            let ParseError(es) = err;
            List.last(es)

        pub def latestLoc(err: ParseError): Option[Location] =
            err |> latest |> Option.map(fst)

        def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
            s
                |> List.groupBy(fst)
                |> List.map(xs ->
                    (xs |> Nel.head |> fst, xs |> Nel.map(snd) |> Nel.join("; "))
                )
                |> List.sortBy(t ->
                    let Location({offset | _}) = fst(t);
                    offset
                )

        def formatLoc(loc: Location): String =
            "${Location.line(loc)}.${Location.col(loc)}"
    }

    mod Examples {
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
                        _ -> n,
                        P.listOfN(n, P.char('a'))
                    ),
                nonNegativeInt()
            )
    }
}
