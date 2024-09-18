--sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons True) = (Cons False)
negar (Cons False) = (Cons True)
negar (Not f) = f
negar (And f1 f2) = (Or (negar f1) (negar f2))
negar (Or f1 f2) = (And (negar f1) (negar f2))
negar (Impl f1 f2) = (And f1 (negar f2))
negar (Syss f1 f2)= negar (And (Impl f1 f2) (Impl f2 f1))

-- Ejercicio 1.
fnn :: Prop -> Prop
fnn (Var x) = Var x  
fnn (Cons b) = Cons b 
fnn (Not p) = case p of -- Usé esto, no se si esté permitido lol. 
    Not p' -> fnn p' -- Si p == "Not p" entonces aplicamos fnn a p solita (Doble negación).
    And p q -> Or (fnn (Not p)) (fnn (Not q))  -- Si p es un And, entonces hacemos De Morgan para AND
    Or p q -> And (fnn (Not p)) (fnn (Not q))  -- Si p es un Or, entonces hacemos De Morgan para OR
    _      -> Not (fnn p)  -- En cualquier otro caso solo regresamos la negacion de fnn de p :)
fnn (And p q) = And (fnn p) (fnn q)  
fnn (Or p q) = Or (fnn p) (fnn q)  
fnn (Impl p q) = fnn (Or (Not p) q)  
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))

-- Función auxiliar, distribuir aplica leyes de distribución.
-- φ ∨ (ρ ∧ ψ) ≡ (φ ∨ ρ) ∧ (φ ∨ ψ) | φ ∧ (ρ ∨ ψ) ≡ (φ ∧ ρ) ∨ (φ ∧ ψ)
distribuir :: Prop -> Prop
distribuir (Or p (And q r)) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or (And q r) p) = And (distribuir (Or q p)) (distribuir (Or r p))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir p = p

-- Ejercicio 2.
fnc :: Prop -> Prop
fnc p = fnn (distribuir p)

-- Ejercicio 3.
type Literal = Prop

-- Ejercicio 4.
type Clausula = [Literal]

-- Ejercicio 5.
--Función Auxiliar.
extraer :: Prop -> [Literal]
extraer (Or p q) = extraer p ++ extraer q
extraer p = [p]
duplicados [] = []
duplicados (x:xs) = x : duplicados (filter (/= x) xs)

clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q
clausulas (Or p q) = [duplicados(extraer (Or p q))]
clausulas p = [[p]]

--Ejercicio 6
--Función Auxiliar.
elimina :: Eq a => a -> [a] ->  [a]
elimina _ [] = []
elimina y (x:xs) = if y == x then xs else [x] ++ (elimina y xs)

--Función auxiliar
eliminarDuplicados :: (Eq a) => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = if x `elem` xs then eliminarDuplicados xs else [x] ++ (eliminarDuplicados xs)

--Función auxiliar para resolucion sin eliminar elementos duplicados
resolucionAux :: Clausula -> Clausula -> Clausula
resolucionAux [] ys = ys
resolucionAux ((Var p):xs) ys = if (Not (Var p)) `elem` ys
    then xs ++ (elimina ((Not (Var p))) ys)
    else [Var p] ++ resolucionAux xs ys
resolucionAux ((Not (Var p)):xs) ys = if (Var p) `elem` ys
    then xs ++ (elimina (Var p) ys)
    else [Not (Var p)] ++ resolucionAux xs ys

--Función resolución
resolucion :: Clausula -> Clausula -> Clausula
resolucion xs ys = eliminarDuplicados (resolucionAux xs ys)

--Ejercicio 7
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente xs ys = if resolucion xs ys == [] 
    then False 
    else True


