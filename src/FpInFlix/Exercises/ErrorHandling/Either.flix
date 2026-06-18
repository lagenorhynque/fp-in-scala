pub mod FpInFlix.Exercises.ErrorHandling.Either {
    pub enum Either[e, a] with Eq, ToString {
        case Left(e)
        case Right(a)
    }

    pub def map(f: a -> b \ ef, et: Either[e, a]): Either[e, b] \ ef =
        match et {
            case Left(x) => Left(x)
            case Right(y) => Right(f(y))
        }


    pub def flatMap(f: a -> Either[e, b] \ ef, et: Either[e, a]): Either[e, b] \ ef =
        match et {
            case Left(x) => Left(x)
            case Right(y) => f(y)
        }

}
