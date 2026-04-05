// A comment!
/* Another comment! */
pub mod FpInFlix.Exercises.GettingStarted.MyProgram {
    pub def abs(x: Int32): Int32 =
        if (x < 0) -x
        else x

    def formatAbs(x: Int32): String =
        "The absolute value of ${x} is ${abs(x)}"

    pub def printAbs(): Unit \ IO =
        println(formatAbs(-42))

    // A definition of factorial, using a local, tail recursive function
    pub def factorial(n: Int32): Int32 =
        @Tailrec
        def go(x: Int32, acc: Int32): Int32 =
            if (x <= 0) acc
            else go(x - 1, x * acc);
        go(n, 1)

    // Another implementation of `factorial`, this time with a `while` loop
    pub def factorial2(n: Int32): Int32 =
        region rc {
            let acc = Ref.fresh(rc, 1);
            foreach (i <- Range.Range(1, n + 1))
                Ref.transform(x -> x * i, acc);
            Ref.get(acc)
        }
}
