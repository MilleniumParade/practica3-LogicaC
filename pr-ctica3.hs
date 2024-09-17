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

-- Ejercicio q.
fnn :: Prop -> Prop
fnn (Var x) = Var x  
fnn (Cons b) = Cons b 
fnn (Not p) = case p of -- Usé esto, no se si esté permitido, lol
    Not p' -> fnn p' -- Si p es (Not p) entonces fnn de p (pues doble negación)
    _      -> Not (fnn p) -- En cualquier otro caso, hacemos fnn de p y aplicamos negación.
fnn (And p q) = And (fnn p) (fnn q)  
fnn (Or p q) = Or (fnn p) (fnn q)  
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))  
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q)) 
fnn (Impl p q) = fnn (Or (Not p) q)  
fnn (Syss p q) = fnn (Or (And p q) (And (Not p) (Not q)))  


