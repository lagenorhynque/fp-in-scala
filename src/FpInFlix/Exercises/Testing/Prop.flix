pub mod FpInFlix.Exercises.Testing.Prop {
    use FpInFlix.Exercises.State.{Rng, State}
    use FpInFlix.Exercises.Testing.Gen

    enum Prop((MaxSize, TestCases, Rng) -> Result.Result)

    mod SuccessCount {
        pub enum SuccessCount(Int32) with Eq, ToString

        pub def toInt(x: SuccessCount): Int32 =
            let SuccessCount(n) = x;
            n

        pub def fromInt(x: Int32): SuccessCount =
            SuccessCount(x)
    }

    mod TestCases {
        enum TestCases(Int32) with Eq, ToString

        pub def toInt(x: TestCases): Int32 =
            let TestCases(n) = x;
            n

        pub def fromInt(x: Int32): TestCases =
            TestCases(x)
    }


    mod MaxSize {
        enum MaxSize(Int32) with Eq, ToString

        pub def toInt(x: MaxSize): Int32 =
            let MaxSize(n) = x;
            n

        pub def fromInt(x: Int32): MaxSize =
            MaxSize(x)
    }

    mod FailedCase {
        enum FailedCase(String) with Eq, ToString

        pub def toString(x: FailedCase): String =
            let FailedCase(s) = x;
            s

        pub def fromString(x: String): FailedCase =
            FailedCase(x)
    }

    mod Result {
        use FpInFlix.Exercises.Testing.Prop.{FailedCase, SuccessCount}

        pub enum Result {
            case Passed
            case Falsified({failure = FailedCase, successes = SuccessCount})
            case Proved
        }

        instance ToString[Result] {
            pub def toString(r: Result): String =
                match r {
                    case Passed => "Passed"
                    case Falsified({failure, successes}) =>
                        "Falsified({failure = ${failure}, successes = ${successes}})"
                    case Proved => "Proved"
                }
        }

        pub def isFalsified(r: Result): Bool =
            match r {
                case Passed => false
                case Falsified(_) => true
                case Proved => false
            }
    }

    def randomLazyList(ga: Gen[a], rng: Rng): DelayList[a] =
        let Gen.Gen(sa) = ga;
        let (a, rng2) = State.runState(rng, sa);
        DelayList.LCons(a, lazy randomLazyList(ga, rng2))

    pub def forAll(ga: Gen[a], f: a -> Bool): Prop with ToString[a] =
        Prop.make((n, rng) ->
            randomLazyList(ga, rng)
                |> DelayList.zipWithIndex
                |> DelayList.take(n)
                |> DelayList.map(t ->
                    let (i, a) = t;
                    if (f(a)) Result.Passed
                    else Result.Falsified({
                        failure = FailedCase.fromString("${a}"),
                        successes = SuccessCount.fromInt(i)
                    })
                )
                |> DelayList.findLeft(Result.isFalsified)
                |> Option.getWithDefault(Result.Passed)
        )

    pub def forAllSized(sga: Gen.SGen[a], f: a -> Bool): Prop with ToString[a] =
        Prop((max, n, rng) ->
            let Gen.SGen(sa) = sga;
            let casesPerSize = TestCases.fromInt((TestCases.toInt(n) - 1) / MaxSize.toInt(max) + 1);
            let props = DelayList.startFrom(0)
                |> DelayList.take(Int32.min(TestCases.toInt(n), MaxSize.toInt(max)) + 1)
                |> DelayList.map(i -> forAll(sa(i), f));
            let prop = props
                |> DelayList.map(p ->
                    Prop((max2, _, rng2) ->
                        let Prop(g) = p;
                        g(max2, casesPerSize, rng2)
                    )
                )
                |> DelayList.reduceLeft((&&));
            let Prop(h) = unchecked_cast(Option.getOrAbort(prop) as _ \ {});
            h(max, n, rng)
        )

    pub def make(f: (Int32, Rng) -> Result): Prop =
        Prop((_, n, rng) -> f(TestCases.toInt(n), rng))

    pub def &&(p1: Prop, p2: Prop): Prop =
        Prop((max, n, rng) ->
            let Prop(f1) = p1 |> Prop.tag("and-left");
            let result1 = f1(max, n, rng);
            if (Result.isFalsified(result1)) result1
            else {
                let Prop(f2) = p2 |> Prop.tag("and-right");
                f2(max, n, rng)
            }
        )

    pub def ||(p1: Prop, p2: Prop): Prop =
        Prop((max, n, rng) ->
            let Prop(f1) = p1 |> Prop.tag("or-left");
            match f1(max, n, rng) {
                case Result.Falsified({failure | _}) => {
                    let Prop(f2) = p2
                        |> Prop.tag("or-right")
                        |> Prop.tag(FailedCase.toString(failure));
                    f2(max, n, rng)
                }
                case x => x
            }
        )

    pub def tag(msg: String, p: Prop): Prop =
        Prop((max, n, rng) ->
            let Prop(f) = p;
            match f(max, n, rng) {
                case Result.Falsified({failure, successes}) =>
                    Result.Falsified({
                        failure = FailedCase.fromString("${msg}(${FailedCase.toString(failure)})"),
                        successes = successes
                    })
                case x => x
            }
        )

    pub def check(p: Prop): Result \ Time.Clock =
        let Prop(f) = p;
        let maxSize = MaxSize.fromInt(100);
        let testCases = TestCases.fromInt(100);
        let rng = Rng.Rng(Time.Clock.now());
        f(maxSize, testCases, rng)

    pub def runProp(p: Prop): Unit \ {IO, Time.Clock} =
        let Prop(f) = p;
        let maxSize = MaxSize.fromInt(100);
        let testCases = TestCases.fromInt(100);
        let rng = Rng.Rng(Time.Clock.now());
        match f(maxSize, testCases, rng) {
            case Result.Passed =>
                println("+ OK, passed ${TestCases.toInt(testCases)} tests.")
            case Result.Falsified({failure, successes}) =>
                println("! Falsified after ${SuccessCount.toInt(successes)} passed tests:\n ${FailedCase.toString(failure)}")
            case Result.Proved =>
                println("+ OK, proved property.")
        }
}
