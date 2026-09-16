pub mod FpInFlix.Exercises.Parsing.Parsers.ParseError {
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
