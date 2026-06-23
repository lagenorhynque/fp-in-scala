pub mod FpInFlix.Exercises.Parallelism.Par {
    import java.lang.Runnable
    import java.util.concurrent.{CompletableFuture, ExecutorService, Executors, Future}

    enum Par[a](ExecutorService -> Future[a] \ IO)

    pub def runPar(es: ExecutorService, pa: Par[a]): Future[a] \ IO =
        let Par(f) = pa;
        f(es)

    pub def unit(a: a): Par[a] =
        Par(_es -> unitFuture(a))

    def unitFuture(a: a): Future[a] \ IO =
        checked_cast(CompletableFuture.completedFuture(a))

    /*
    // Scala版の忠実な移植実装: https://github.com/flix/flix/issues/12876 のため正常動作しない
    def unitFuture(a: a): Future[a] \ IO =
        new Future[a] {
            def isDone(_this: Future[a]): Bool = true
            def get(_this: Future[a]): a = a
            def get(_this: Future[a], _timeout: Int64, _unit: TimeUnit): a = a
            def isCancelled(_this: Future[a]): Bool = false
            def cancel(_this: Future[a], _mayInterruptIfRunning: Bool): Bool = false
        }
    */

    pub def map2(f: (a, b) -> c, pa: Par[a], pb: Par[b]): Par[c] =
        Par(es ->
            let Par(fa) = pa;
            let af = fa(es);
            let Par(fb) = pb;
            let bf = fb(es);
            unitFuture(f(af.get(), bf.get()))
        )

    pub def fork(pa: Lazy[Par[a]]): Par[a] =
        Par(es ->
            let cf = (new CompletableFuture(): CompletableFuture[a]);
            let task = new Runnable {
                def $run(_this: Runnable): Unit \ IO =
                    let Par(f) = force pa;
                    let v = f(es).get();
                    discard cf.complete(v)
            };
            discard es.submit(task);
            checked_cast(cf)
        )

    /*
    // Scala版の忠実な移植実装: https://github.com/flix/flix/issues/12876 のため正常動作しない
    pub def fork(pa: Lazy[Par[a]]): Par[a] =
        Par(es ->
            es.submit(new Callable[a] {
                def call(_this: Callable[a]): a \ IO =
                    let Par(f) = force pa;
                    f(es).get()
            })
        )
    */

    pub def lazyUnit(a: Lazy[a]): Par[a] =
        fork(lazy unit(force a))

    pub def asyncF(f: a -> b): a -> Par[b] =
        x -> lazyUnit(lazy f(x))

    pub def map(f: a -> b, pa: Par[a]): Par[b] =
        map2((a, _) -> f(a), pa, unit(()))

    pub def sortPar(parList: Par[List[Int32]]): Par[List[Int32]] =
        map(List.sort, parList)

    pub def sequenceSimple(pas: List[Par[a]]): Par[List[a]] =
        List.foldRight((pa, acc) -> map2(List.cons, pa, acc), unit(Nil), pas)

    pub def sequenceRight(pas: List[Par[a]]): Par[List[a]] =
        match pas {
            case Nil => unit(Nil)
            case head :: tail => map2(List.cons, head, fork(lazy sequenceRight(tail)))
        }

    def sequenceBalanced(pas: Vector[Par[a]]): Par[Vector[a]] =
        match Vector.length(pas) {
            case 0 => unit(Vector.empty())
            case 1 => map(a -> Vector#{a}, Vector.get(0, pas))
            case n =>
                let (l, r) = Vector.splitAt(n / 2, pas);
                map2(Vector.append, sequenceBalanced(l), sequenceBalanced(r))
        }

    pub def sequence(pas: List[Par[a]]): Par[List[a]] =
        map(Vector.toList, pas |> List.toVector |> sequenceBalanced)

    pub def parMap(f: a -> b, ps: List[a]): Par[List[b]] =
        fork(
            let fbs = List.map(asyncF(f), ps);
            lazy sequence(fbs)
        )

    pub def flatMap(f: a -> Par[b], pa: Par[a]): Par[b] =
        Par(es ->
            let Par(fa) = pa;
            let a = fa(es).get();
            let Par(fb) = f(a);
            fb(es)
        )

    pub def runWithThreadPool(pa: Par[a]): a \ IO =
        let es = Executors.newFixedThreadPool(4);
        let v = runPar(es, pa).get();
        es.shutdown();
        v
}
