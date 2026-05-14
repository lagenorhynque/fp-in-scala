pub mod FpInFlix.Exercises.GettingStarted.FormatAbsAndFactorial {
    use FpInFlix.Exercises.GettingStarted.MyProgram.{abs, factorial, formatResult}

    // Now we can use our general `formatResult` function
    // with both `abs` and `factorial`
    pub def printAbsAndFactorial(): Unit \ IO =
        println(formatResult("absolute value", -42, abs));
        println(formatResult("factorial", 7, factorial))
}
