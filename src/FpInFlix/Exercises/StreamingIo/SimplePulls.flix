pub mod FpInFlix.Exercises.StreamingIo.SimplePulls {
    mod IteratorExamples {
        import java.io.{BufferedReader, BufferedWriter, Closeable, IOException, PrintWriter, StringWriter}
        import java.nio.file.{Files, Paths}

        def toCelsius(fahrenheit: Float64): Float64 =
            (5.0 / 9.0) * (fahrenheit - 32.0)

        def trimmed(s: String): String = String.trim(s)
        def nonEmpty(s: String): Bool = not String.isEmpty(s)
        def nonComment(s: String): Bool = not String.startsWith(prefix = "#", s)
        def asDouble(s: String): Option[Float64] = Float64.fromString(s)

        def conversion(iter: Iterator[String, ef, r]): Iterator[Float64, ef, r] =
            iter
                |> Iterator.map(trimmed)
                |> Iterator.filter(nonEmpty)
                |> Iterator.filter(nonComment)
                |> Iterator.filterMap(asDouble)
                |> Iterator.map(toCelsius)

        def bufferedReader(file: String): BufferedReader \ IO =
            Files.newBufferedReader(Paths.get(file))
        def bufferedWriter(file: String): BufferedWriter \ IO =
            Files.newBufferedWriter(Paths.get(file))

        def withResource(
            rc: Region[r],
            acquire: Unit -> Closeable \ ef1,
            handle: Closeable -> a \ ef2
        ): Result[IOException, a] \ ef1 + ef2 + r + IO =
            let closable = Ref.fresh(rc, Option.empty());
            let result = try {
                Ref.put(Some(acquire()), closable);
                closable
                    |> Ref.get
                    |> Option.foldLeft((_, c) ->
                        c |> handle |> Ok,
                        Err(new IOException("unexpected"))
                    )
            } catch {
                case e: IOException => Err(e)
            };
            closable |> Ref.get |> Option.forEach(c -> c.close());
            result
        def withReader(
            rc: Region[r],
            file: String,
            handle: BufferedReader -> a \ ef
        ): Result[IOException, a] \ ef + r + IO =
            withResource(
                rc,
                () -> checked_cast(bufferedReader(file)),
                unchecked_cast(handle as Closeable -> a \ ef)
            )
        def withWriter(
            rc: Region[r],
            file: String,
            handle: BufferedWriter -> a \ ef
        ): Result[IOException, a] \ ef + r + IO =
            withResource(
                rc,
                () -> checked_cast(bufferedWriter(file)),
                unchecked_cast(handle as Closeable -> a \ ef)
            )

        def inspectEx(result: Result[IOException, a]): Result[String, a] \ IO =
            Result.mapErr(e ->
                let sw = new StringWriter();
                let pw = new PrintWriter(sw);
                e.printStackTrace(pw);
                sw.toString(),
                result
            )

        def lineIterator(rc: Region[r], reader: BufferedReader): Iterator[String, r + IO, r] \ r =
            Iterator.iterate(rc, _ ->
                let line = reader.readLine();
                if (Object.isNull(line)) None
                else Some(line),
                ""
            )

        pub def convert(inputFile: String, outputFile: String): Result[String, Unit] \ IO =
            region rc {
                withReader(rc, inputFile, reader ->
                    withWriter(rc, outputFile, writer ->
                        lineIterator(rc, reader)
                            |> conversion
                            |> Iterator.forEach(a -> {
                                writer.write("${a}");
                                writer.newLine()
                            })
                    )
                )
            }
                |> Result.flatten
                |> inspectEx
    }
}
