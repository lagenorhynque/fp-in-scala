pub mod FpInFlix.Exercises.ErrorHandling.Validated {
    pub enum Validated[e, a] with Eq, ToString {
        case Valid(a)
        case Invalid(Nel[e])
    }
}
