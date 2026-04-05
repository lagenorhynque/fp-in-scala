mod FpInFlix.Exercises.GettingStarted.TestMyProgram {
    use Assert.{assertEq};
    use FpInFlix.Exercises.GettingStarted.MyProgram.{factorial, factorial2};

    @Test
    def testFactorial(): Unit \ Assert =
        assertEq(expected = 1, factorial(0));
        assertEq(expected = 1, factorial(1));
        assertEq(expected = 2, factorial(2));
        assertEq(expected = 6, factorial(3));
        assertEq(expected = 24, factorial(4));
        assertEq(expected = 120, factorial(5))

    @Test
    def testFactorial2(): Unit \ Assert =
        assertEq(expected = 1, factorial2(0));
        assertEq(expected = 1, factorial2(1));
        assertEq(expected = 2, factorial2(2));
        assertEq(expected = 6, factorial2(3));
        assertEq(expected = 24, factorial2(4));
        assertEq(expected = 120, factorial2(5))
}
