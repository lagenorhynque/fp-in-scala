pub mod FpInFlix.Exercises.Parsing.Reference {
    use FpInFlix.Exercises.Parsing.Parsers
    use FpInFlix.Exercises.Parsing.Parsers.{Location, ParseError}
    use FpInFlix.Exercises.Parsing.Reference.Parser.Parser
    use FpInFlix.Exercises.Parsing.Reference.ParseResult.{Failure, Success}

    pub enum Parser[a](Location -> ParseResult[a])

    mod ParseResult {
        use FpInFlix.Exercises.Parsing.Parsers.ParseError

        pub enum ParseResult[a] {
            case Success(a, Int32)
            case Failure(ParseError, Bool)
        }

        pub def extract(r: ParseResult[a]): Result[ParseError, a] =
            match r {
                case Success(a, _) => Ok(a)
                case Failure(e, _) => Err(e)
            }

        pub def uncommit(r: ParseResult[a]): ParseResult[a] =
            match r {
                case Failure(e, true) => Failure(e, false)
                case _ => r
            }

        pub def addCommit(isCommitted: Bool, r: ParseResult[a]): ParseResult[a] =
            match r {
                case Failure(e, c) => Failure(e, c or isCommitted)
                case _ => r
            }

        pub def mapError(f: ParseError -> ParseError, r: ParseResult[a]): ParseResult[a] =
            match r {
                case Failure(e, c) => Failure(f(e), c)
                case _ => r
            }

        pub def advanceSuccess(n: Int32, r: ParseResult[a]): ParseResult[a] =
            match r {
                case Success(a, m) => Success(a, n + m)
                case _ => r
            }
    }

    instance Parsers[Parser] {
        pub def succeed(a: a): Parser[a] =
            Parser(_ -> Success(a, 0))

        pub def string(s: String): Parser[String] =
            Parser(loc ->
                let Location({input, offset}) = loc;
                let i = firstNonMatchingIndex(input, s, offset);
                if (i == -1) Success(s, String.length(s))
                else Failure(loc |> Location.advanceBy(i) |> Location.toError("'${s}'"), i != 0)
            )

        pub def regex(r: Regex): Parser[String] =
            Parser(loc ->
                Option.foldLeft(
                    (_, m) -> Success(m, String.length(m)),
                    Failure(Location.toError("regex ${Regex.pattern(r)}", loc), false),
                    Regex.getPrefix(r, Location.remaining(loc))
                )
            )

        pub def fail(msg: String): Parser[a] =
            Parser(loc -> Failure(Location.toError(msg, loc), true))

        pub def runParser(s: String, p: Parser[a]): Result[ParseError, a] =
            let Parser(f) = p;
            Location({input = s, offset = 0}) |> f |> ParseResult.extract

        pub def orElse(p1: Parser[a], p2: Lazy[Parser[a]]): Parser[a] =
            Parser(loc ->
                let Parser(f1) = p1;
                match f1(loc) {
                    case Failure(_, false) =>
                        let Parser(f2) = force p2;
                        f2(loc)
                    case r => r
                }
            )

        pub def attempt(p: Parser[a]): Parser[a] =
            Parser(loc ->
                let Parser(f) = p;
                loc |> f |> ParseResult.uncommit
            )

        pub def flatMap(f: a -> Parser[b], p: Parser[a]): Parser[b] =
            Parser(loc ->
                let Parser(g) = p;
                match g(loc) {
                    case Success(a, n) =>
                        let Parser(h) = f(a);
                        loc
                            |> Location.advanceBy(n)
                            |> h
                            |> ParseResult.addCommit(n != 0)
                            |> ParseResult.advanceSuccess(n)
                    case Failure(e, c) => Failure(e, c)
                }
            )

        pub def slice(p: Parser[a]): Parser[String] =
            Parser(loc ->
                let Parser(f) = p;
                match f(loc) {
                    case Success(_, n) => Success(Location.slice(n, loc), n)
                    case Failure(e, c) => Failure(e, c)
                }
            )

        pub redef many(p: Parser[a]): Parser[List[a]] =
            Parser(loc ->
                let Parser(f) = p;
                region rc {
                    let buf = MutList.empty(rc);
                    @Tailrec
                    def go(offset) =
                        match f(Location.advanceBy(offset, loc)) {
                            case Success(a, n) =>
                                MutList.push(a, buf);
                                go(offset + n)
                            case Failure(e, true) => Failure(e, true)
                            case _ => Success(MutList.toList(buf), offset)
                        };
                    go(0)
                }
            )

        pub def scope(msg: String, p: Parser[a]): Parser[a] =
            Parser(loc ->
                let Parser(f) = p;
                loc |> f |> ParseResult.mapError(ParseError.push(loc, msg))
            )

        pub def label(msg: String, p: Parser[a]): Parser[a] =
            Parser(loc ->
                let Parser(f) = p;
                loc |> f |> ParseResult.mapError(ParseError.label(msg))
            )
    }

    def firstNonMatchingIndex(s1: String, s2: String, offset: Int32): Int32 =
        @Tailrec
        def loop(i: Int32): Int32 =
            if (i + offset < String.length(s1) and i < String.length(s2))
                if (String.charAt(i + offset, s1) != String.charAt(i, s2)) i
                else loop(i + 1)
            else if (String.length(s1) - offset >= String.length(s2)) -1
            else String.length(s1) - offset;
        loop(0)
}
