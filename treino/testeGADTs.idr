
data Expr : Type -> Type where
    I : Int -> Expr Int
    B : Bool -> Expr Bool
