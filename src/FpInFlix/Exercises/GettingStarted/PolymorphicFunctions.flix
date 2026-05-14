pub mod FpInFlix.Exercises.GettingStarted.PolymorphicFunctions {
    // Here's a polymorphic version of `findFirst`, parameterized on
    // a function for testing whether an `a` is the element we want to find.
    // Instead of hard-coding `String`, we take a type `a` as a parameter.
    // And instead of hard-coding an equality check for a given key,
    // we take a function with which to test each element of the vector.
    pub def findFirst(xs: Vector[a], p: a -> Bool): Int32 =
        @Tailrec
        def loop(n: Int32): Int32 =
            if (n >= Vector.size(xs)) -1
            // If the function `p` matches the current element,
            // we've found a match and we return its index in the array.
            else if (xs |> Vector.get(n) |> p) n
            else loop(n + 1);
        loop(0)

    // Exercise 2.2: Implement a polymorphic function to check whether
    // an `Vector[a]` is sorted
    pub def isSorted(xs: Vector[a], gt: (a, a) -> Bool): Bool =
        @Tailrec
        def go(n: Int32): Bool =
            if (n >= Vector.size(xs) - 1) true
            else if (gt(Vector.get(n, xs), Vector.get(n + 1, xs))) false
            else go(n + 1);
        go(0)

    // Polymorphic functions are often so constrained by their type
    // that they only have one implementation! Here's an example:

    pub def partial1(a: a, f: (a, b) -> c): b -> c =
        b -> f(a, b)


    // Exercise 2.3: Implement `curry`.

    // Note that `->` associates to the right, so we could
    // write the return type as `a -> b -> c`
    pub def curry(f: (a, b) -> c): a -> (b -> c) =
        a -> b -> f(a, b)

    // Exercise 2.4: Implement `uncurry`
    pub def uncurry(f: a -> b -> c): (a, b) -> c =
        (a, b) -> f(a)(b)

    /*
     * Note that we can go back and forth between the two forms. We can curry
     * and uncurry and the two forms are in some sense "the same". In FP jargon,
     * we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
     * a term we inherit from category theory.
     */

    // Exercise 2.5: Implement `compose`

    pub def compose(f: b -> c, g: a -> b): a -> c =
        a -> a |> g |> f
}
