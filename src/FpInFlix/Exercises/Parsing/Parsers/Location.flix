pub mod FpInFlix.Exercises.Parsing.Parsers.Location {
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
        else
            ""

    pub def columnCaret(loc: Location): String =
        "${String.repeat(Location.col(loc) - 1, " ")}^"
}
