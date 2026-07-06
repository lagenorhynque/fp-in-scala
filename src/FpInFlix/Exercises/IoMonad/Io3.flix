pub mod FpInFlix.Exercises.IoMonad.Io3 {
    mod Free {
        pub enum Free[f: Type -> Type, a: Type] {
            case Pure(a)
            case Free(f[Free[f, a]])
        }

        instance Functor[Free[f]] with Functor[f] {
            pub def map(f: a -> b \ ef, x: Free[f, a]): Free[f, b] \ ef =
                match x {
                    case Pure(a) => Pure(f(a))
                    case Free(fa) =>
                        Free(Functor.map((a: Free[f, a]) -> Functor.map(f, a), fa))
                }
        }

        instance Applicative[Free[f]] with Functor[f] {
            pub def point(x: a): Free[f, a] =
                Pure(x)
            pub def ap(f: Free[f, a -> b \ ef], x: Free[f, a]): Free[f, b] \ ef =
                Monad.flatMap(g -> Functor.map(g, x), f)
        }

        instance Monad[Free[f]] with Functor[f] {
            pub def flatMap(f: a -> Free[f, b] \ ef, x: Free[f, a]): Free[f, b] \ ef =
                match x {
                    case Pure(a) => f(a)
                    case Free(g) =>
                        Free(Functor.map((fa: Free[f, a]) -> Monad.flatMap(f, fa), g))
                }
        }

        pub def liftF(fa: f[a]): Free[f, a] with Functor[f] =
            Free(Functor.map(a -> Pure(a), fa))
    }

    mod Console {
        use FpInFlix.Exercises.IoMonad.Io3.Free
        use FpInFlix.Exercises.IoMonad.Io3.fahrenheitToCelsius

        pub enum Console[next] {
            case ReadLine(Option[String] -> next)
            case PrintLine(String, next)
        }

        instance Functor[Console] {
            pub def map(f: a -> b \ ef, fa: Console[a]): Console[b] \ ef =
                match fa {
                    case ReadLine(g) => ReadLine(s -> unchecked_cast(f(g(s)) as _ \ {}))
                    case PrintLine(s, next) => PrintLine(s, f(next))
                }
        }

        pub def readLn(): Free[Console, Option[String]] =
            Free.liftF(ReadLine(identity))
        pub def printLn(line: String): Free[Console, Unit] =
            Free.liftF(PrintLine(line, ()))

        @Tailrec
        pub def interpretWithSysConsole(expr: Free[Console, a]): a \ Sys.Console =
            match expr {
                case Free.Pure(a) => a
                case Free.Free(ReadLine(next)) =>
                    interpretWithSysConsole(next(Sys.Console.readln()))
                case Free.Free(PrintLine(line, next)) =>
                    Sys.Console.println(line);
                    interpretWithSysConsole(next)
            }

        pub def interpretWithBuffer(in: List[String], expr: Free[Console, a]): (a, List[String]) =
            @Tailrec
            def loop(x, inBuf, outBuf) =
                match x {
                    case Free.Pure(a) => (a, List.reverse(outBuf))
                    case Free.Free(ReadLine(next)) =>
                        match inBuf {
                            case Nil => loop(next(None), Nil, outBuf)
                            case s :: ss => loop(next(Some(s)), ss, outBuf)
                        }
                    case Free.Free(PrintLine(line, next)) =>
                        loop(next, inBuf, line :: outBuf)
                };
            loop(expr, in, Nil)

        pub def exampleExpr(): Free[Console, Unit] =
            forM(
                _ <- printLn("Enter a temperature in degrees Fahrenheit: ");
                d <- readLn() |> Functor.map(Option.flatMap(Float64.fromString));
                _ <- printLn("In degrees Celsius: ${Option.map(fahrenheitToCelsius, d)}")
            ) yield ()
    }

    mod ConsoleEff {
        use FpInFlix.Exercises.IoMonad.Io3.fahrenheitToCelsius

        pub eff Console {
            def readLn(): Option[String]
            def printLn(line: String): Unit
        }

        pub def runWithSysConsole(f: Unit -> a \ ef): a \ ef - Console + Sys.Console =
            run {
                f()
            } with handler Console {
                def readLn(resume) =
                    resume(Sys.Console.readln())
                def printLn(line, resume) = {
                    Sys.Console.println(line);
                    resume()
                }
            }

        pub def runWithBuffer(in: List[String], f: Unit -> a \ ef): (a, List[String]) \ ef - Console =
            region rc {
                let inBuf = Ref.fresh(rc, in);
                let outBuf = Ref.fresh(rc, Nil);
                let a = run {
                    f()
                } with handler Console {
                    def readLn(resume) =
                        match Ref.get(inBuf) {
                            case Nil => resume(None)
                            case s :: ss =>
                                Ref.put(ss, inBuf);
                                resume(Some(s))
                        }
                    def printLn(line, resume) = {
                        Ref.put(line :: Ref.get(outBuf), outBuf);
                        resume()
                    }
                };
                (a, outBuf |> Ref.get |> List.reverse)
            }

        pub def exampleFn(): Unit \ Console =
            Console.printLn("Enter a temperature in degrees Fahrenheit: ");
            let d = Console.readLn() |> Option.flatMap(Float64.fromString);
            Console.printLn("In degrees Celsius: ${Option.map(fahrenheitToCelsius, d)}")
    }

    pub def fahrenheitToCelsius(f: Float64): Float64 =
        (f - 32.0) * 5.0 / 9.0
}
