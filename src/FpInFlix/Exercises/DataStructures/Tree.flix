pub mod FpInFlix.Exercises.DataStructures.Tree {
    pub enum Tree[a] with Eq, ToString {
        case Leaf(a)
        case Branch(Tree[a], Tree[a])
    }

    pub def size(t: Tree[a]): Int32 =
        match t {
            case Leaf(_) => 1
            case Branch(left, right) => 1 + size(left) + size(right)
        }

     pub def depth(t: Tree[a]): Int32 =
        match t {
            case Leaf(_) => 1
            case Branch(left, right) => 1 + Int32.max(depth(left), depth(right))
        }
}
