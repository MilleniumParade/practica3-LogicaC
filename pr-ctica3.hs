data Prop = Var String | Cons Bool | Not Prop
| And Prop Prop | Or Prop Prop
| Impl Prop Prop | Syss Prop Prop
deriving (Eq)

instance Show Prop where 
    show Cons True = "Verdadero"
    show Cons False = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"
    
negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons p) = Cons (not p)
negar (Or p q) = And (negar p) (negar q)
negar (And p q) = Or (negar p) (negar q)
negar (Impl p q) = And p (negar q)
negar (Syss p q) = Not (And (Impl p q) (Impl q p))