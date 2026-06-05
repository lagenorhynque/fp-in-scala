pub mod FpInFlix.Exercises.GettingStarted.MonomorphicBinarySearch {
    // First, a findFirst, specialized to `String`.
    // Ideally, we could generalize this to work for any `Vector` type.
    pub def findFirst(ss: Vector[String], key: String): Int32 =
        @Tailrec
        def loop(n) =
            // If `n` is past the end of the vector, return `-1`
            // indicating the key doesn't exist in the vector.
            if (n >= Vector.size(ss)) -1
            // `Vector.get(n, ss)` extracts the n'th element of the vector `ss`.
            // If the element at `n` is equal to the key, return `n`
            // indicating that the element appears in the vector at that index.
            else if (Vector.get(n, ss) == key) n
            else loop(n + 1);  // Otherwise increment `n` and keep looking.
        // Start the loop at the first element of the vector.
        loop(0)
}
