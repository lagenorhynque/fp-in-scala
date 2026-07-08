pub mod FpInFlix.Exercises.LocalEffects.St {
    enum St[s, a](s -> (a, s))

    instance Functor[St[s]] {
        pub def map(f: a -> b \ ef, x: St[s, a]): St[s, b] \ ef =
            checked_ecast(St(s ->
                let St(g) = x;
                let (a, s1) = g(s);
                unchecked_cast((f(a), s1) as _ \ {})
            ))
    }

    instance Applicative[St[s]] {
        pub def point(a: a): St[s, a] =
            St(s -> (a, s))
        pub def ap(f: St[s, a -> b \ ef], x: St[s, a]): St[s, b] \ ef =
            Monad.flatMap(g -> Functor.map(g, x), f)
    }

    instance Monad[St[s]] {
        pub def flatMap(f: a -> St[s, b] \ ef, x: St[s, a]): St[s, b] \ ef =
            checked_ecast(St(s ->
                let St(g) = x;
                let (a, s1) = g(s);
                let St(h) = unchecked_cast(f(a) as _ \ {});
                h(s1)
            ))
    }

    pub def lift(f: s -> (a, s)): St[s, a] =
        St(f)

    pub def runSt(st: Unit -> St[s, a]): a =
        let St(f) = unchecked_cast(st(()) as St[Unit, a]);
        () |> f |> fst

    mod StRef {
        use FpInFlix.Exercises.LocalEffects.St

        struct StRef[_s: Type, a: Type, r: Region] {
            mut cell: a
        }

        pub def read(r: StRef[s, a, r]): St[s, a] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast((r->cell, s) as _ \ {})
            ))

        pub def write(a: Lazy[a], r: StRef[s, a, r]): St[s, Unit] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast({
                    r->cell = force a;
                    ((), s)
                } as _ \ {})
            ))

        pub def make(rc: Region[r], a: a): St[s, StRef[s, a, r]] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast({
                    let r = new StRef @ rc {cell = a};
                    (r, s)
                } as _ \ {})
            ))
    }

    mod StArray {
        use FpInFlix.Exercises.LocalEffects.St

        struct StArray[_s: Type, a: Type, r: Region] {
            mut value: Array[a, r]
        }

        pub def size(arr: StArray[s, a, r]): St[s, Int32] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast((Array.size(arr->value), s) as _ \ {})
            ))

        pub def write(i: Int32, a: a, arr: StArray[s, a, r]): St[s, Unit] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast({
                    Array.put(a, i, arr->value);
                    ((), s)
                } as _ \ {})
            ))

        pub def read(i: Int32, arr: StArray[s, a, r]): St[s, a] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast((Array.get(i, arr->value), s) as _ \ {})
            ))

        pub def freeze(arr: StArray[s, a, r]): St[s, List[a]] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast((Array.toList(arr->value), s) as _ \ {})
            ))

        pub def fill(xs: Map[Int32, a], arr: StArray[s, a, r]): St[s, Unit] \ r =
            Map.foldRightWithKey((k, v, st) ->
                Monad.flatMap(_ -> write(k, v, arr), st),
                Applicative.point(()),
                xs
            )

        pub def swap(i: Int32, j: Int32, arr: StArray[s, a, r]): St[s, Unit] \ r =
            forM(
                x <- read(i, arr);
                y <- read(j, arr);
                _ <- write(i, y, arr);
                _ <- write(j, x, arr)
            ) yield ()

        pub def make(rc: Region[r], size: Int32, value: a): St[s, StArray[s, a, r]] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast({
                    let arr = new StArray @ rc {value = Array.init(rc, constant(value), size)};
                    (arr, s)
                } as _ \ {})
            ))

        pub def fromList(rc: Region[r], xs: List[a]): St[s, StArray[s, a, r]] \ r =
            checked_ecast(St.lift(s ->
                unchecked_cast({
                    let arr = new StArray @ rc {value = List.toArray(rc, xs)};
                    (arr, s)
                } as _ \ {})
            ))
    }

    mod Immutable {
        use FpInFlix.Exercises.LocalEffects.St
        use FpInFlix.Exercises.LocalEffects.St.{StArray, StRef}

        def partition(rc: Region[r], l: Int32, r: Int32, pivot: Int32, arr: StArray[s, Int32, r]): St[s, Int32] \ r =
            forM(
                vp <- StArray.read(pivot, arr);
                _ <- StArray.swap(pivot, r, arr);
                j <- StRef.make(rc, l);
                _ <- List.foldLeft((s, i) ->
                    forM (
                        _ <- s;
                        vi <- StArray.read(i, arr);
                        _ <-
                            if (vi < vp)
                                forM(
                                    vj <- StRef.read(j);
                                    _ <- StArray.swap(i, vj, arr);
                                    _ <- StRef.write(lazy (vj + 1), j)
                                ) yield ()
                            else Applicative.point(())
                    ) yield (),
                    Applicative.point(()),
                    List.range(l, r)
                );
                x <- StRef.read(j);
                _ <- StArray.swap(x, r, arr)
            ) yield x

        def qs(rc: Region[r], l: Int32, r: Int32, arr: StArray[s, Int32, r]): St[s, Unit] \ r =
            if (l < r)
                forM(
                    pi <- partition(rc, l, r, l + (r - l) / 2, arr);
                    _ <- qs(rc, l, pi - 1, arr);
                    _ <- qs(rc, pi + 1, r, arr)
                ) yield ()
            else Applicative.point(())

        pub def quickSort(xs: List[Int32]): List[Int32] =
            if (List.isEmpty(xs)) xs
            else
                St.runSt(() ->
                    region rc {
                        forM(
                            arr <- StArray.fromList(rc, xs);
                            size <- StArray.size(arr);
                            _ <- qs(rc, 0, size - 1, arr);
                            sorted <- StArray.freeze(arr)
                        ) yield sorted
                    }
                )
    }

    mod MyRef {
        struct MyRef[a: Type, r: Region] {
            mut cell: a
        }

        pub def make(rc: Region[r], cell: a): MyRef[a, r] \ r =
            new MyRef @ rc {cell = cell}

        pub def read(r: MyRef[a, r]): a \ r =
            r->cell

        pub def write(a: a, r: MyRef[a, r]): Unit \ r =
            r->cell = a
    }

    mod MyArray {
        struct MyArray[a: Type, r: Region] {
            mut value: Array[a, r]
        }

        pub def size(arr: MyArray[a, r]): Int32 \ r =
            Array.size(arr->value)

        pub def write(i: Int32, a: a, arr: MyArray[a, r]): Unit \ r =
            Array.put(a, i, arr->value)

        pub def read(i: Int32, arr: MyArray[a, r]): a \ r =
            Array.get(i, arr->value)

        pub def freeze(arr: MyArray[a, r]): List[a] \ r =
            Array.toList(arr->value)

        pub def fill(xs: Map[Int32, a], arr: MyArray[a, r]): Unit \ r =
            Map.foldRightWithKey((k, v, _) -> write(k, v, arr), (), xs)

        pub def swap(i: Int32, j: Int32, arr: MyArray[a, r]): Unit \ r =
            Array.swap(i, j, arr->value)

        pub def make(rc: Region[r], size: Int32, value: a): MyArray[a, r] \ r =
            new MyArray @ rc {value = Array.init(rc, constant(value), size)}

        pub def fromList(rc: Region[r], xs: List[a]): MyArray[a, r] \ r =
            new MyArray @ rc {value = List.toArray(rc, xs)}
    }

    mod RegionBasedImmutable {
        use FpInFlix.Exercises.LocalEffects.St.{MyArray, MyRef}

        def partition(rc: Region[r], l: Int32, r: Int32, pivot: Int32, arr: MyArray[Int32, r]): Int32 \ r =
            let vp = MyArray.read(pivot, arr);
            MyArray.swap(pivot, r, arr);
            let j = MyRef.make(rc, l);
            Range.forEach(i ->
                let vi = MyArray.read(i, arr);
                if (vi < vp)
                    let vj = MyRef.read(j);
                    MyArray.swap(i, vj, arr);
                    MyRef.write(vj + 1, j)
                else (),
                Range(l, r)
            );
            let x = MyRef.read(j);
            MyArray.swap(x, r, arr);
            x

        def qs(rc: Region[r], l: Int32, r: Int32, arr: MyArray[Int32, r]): Unit \ r =
            if (l < r)
                let pi = partition(rc, l, r, l + (r - l) / 2, arr);
                qs(rc, l, pi - 1, arr);
                qs(rc, pi + 1, r, arr)
            else ()

        pub def quickSort(xs: List[Int32]): List[Int32] =
            if (List.isEmpty(xs)) xs
            else
                region rc {
                    let arr = MyArray.fromList(rc, xs);
                    let size = MyArray.size(arr);
                    qs(rc, 0, size - 1, arr);
                    MyArray.freeze(arr)
                }
    }
}
