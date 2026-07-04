pub mod FpInFlix.Exercises.Monoids.Monoid {
    pub trait Monoid[a] {
        pub def combine(a1: a, a2: a): a
        pub def empty(): a
    }

    instance Monoid[String] {
        pub def combine(a1: String, a2: String): String =
            "${a1}${a2}"
        pub def empty(): String =
            ""
    }

    pub enum SumInt(Int32) with Eq, ToString

    instance Monoid[SumInt] {
        pub def combine(a1: SumInt, a2: SumInt): SumInt =
            let (SumInt.SumInt(v1), SumInt.SumInt(v2)) = (a1, a2);
            SumInt.SumInt(v1 + v2)
        pub def empty(): SumInt =
            SumInt.SumInt(0)
    }

    pub enum ProductInt(Int32) with Eq, ToString

    instance Monoid[ProductInt] {
        pub def combine(a1: ProductInt, a2: ProductInt): ProductInt =
            let (ProductInt.ProductInt(v1), ProductInt.ProductInt(v2)) = (a1, a2);
            ProductInt.ProductInt(v1 * v2)
        pub def empty(): ProductInt =
            ProductInt.ProductInt(1)
    }

    instance Monoid[List[a]] {
        pub def combine(a1: List[a], a2: List[a]): List[a] =
            a1 ++ a2
        pub def empty(): List[a] =
            List.empty()
    }

    pub enum Or(Bool) with Eq, ToString

    instance Monoid[Or] {
        pub def combine(a1: Or, a2: Or): Or =
            let (Or.Or(v1), Or.Or(v2)) = (a1, a2);
            Or.Or(v1 or v2)
        pub def empty(): Or =
            Or.Or(false)
    }

    pub enum And(Bool) with Eq, ToString

    instance Monoid[And] {
        pub def combine(a1: And, a2: And): And =
            let (And.And(v1), And.And(v2)) = (a1, a2);
            And.And(v1 and v2)
        pub def empty(): And =
            And.And(true)
    }

    pub enum Endo[a](a -> a)

    instance Monoid[Endo[a]] {
        pub def combine(a1: Endo[a], a2: Endo[a]): Endo[a] =
            let (Endo.Endo(f1), Endo.Endo(f2)) = (a1, a2);
            Endo.Endo(f1 >> f2)
        pub def empty(): Endo[a] =
            Endo.Endo(eidentity)
    }

    pub def combineAll(xs: List[a]): a with Monoid[a] =
        List.foldLeft(Monoid.combine, Monoid.empty(), xs)

    pub def foldMap(f: a -> b, xs: List[a]): b with Monoid[b] =
        List.foldLeft((b, a) -> Monoid.combine(b, f(a)), Monoid.empty(), xs)

    instance Monoid[(a, b)] with Monoid[a], Monoid[b] {
        pub def combine(a1: (a, b), a2: (a, b)): (a, b) =
            let ((x1, y1), (x2, y2)) = (a1, a2);
            (Monoid.combine(x1, x2), Monoid.combine(y1, y2))
        pub def empty(): (a, b) =
            (Monoid.empty(), Monoid.empty())
    }
}
