pub mod FpInFlix.Exercises.Introduction.FirstExample {
    mod Coffee {
        pub enum Coffee({price = Float64})

        instance ToString[Coffee] {
            pub def toString(coffee: Coffee): String =
                let Coffee({price}) = coffee;
                "Coffee({price = ${price}})"
        }

        pub def make(): Coffee =
            Coffee({price = 2.0})
    }

    pub def chargeCreditCard(price: Float64): Unit \ IO =
        println("charging ${price}")

    pub def buyCoffee(charge: Float64 -> Unit \ IO): Coffee \ IO =
        let coffee = Coffee.make();
        let Coffee.Coffee({price}) = coffee;
        charge(price);
        coffee

    pub def example(): Unit \ IO =
        let cup = buyCoffee(chargeCreditCard);
        println("a cup of ${cup}")
}
