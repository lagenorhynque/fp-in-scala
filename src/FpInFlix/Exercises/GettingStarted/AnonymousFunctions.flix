// Functions get passed around so often in FP that it's
// convenient to have syntax for constructing a function
// *without* having to give it a name
pub mod FpInFlix.Exercises.GettingStarted.AnonymousFunctions {
    use FpInFlix.Exercises.GettingStarted.MyProgram.{abs, factorial, formatResult}

    // Some examples of anonymous functions:
    pub def printAnonymousFunctions(): Unit \ IO =
        println(formatResult("absolute value", -42, abs));
        println(formatResult("factorial", 7, factorial));
        println(formatResult("increment", 7, (x: Int32) -> x + 1));
        println(formatResult("increment2", 7, (x) -> x + 1));
        println(formatResult("increment3", 7, x -> x + 1));
        println(formatResult("increment4", 7, x -> let r = x + 1; r))
}
