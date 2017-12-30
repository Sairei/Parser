module Expression(
	Variable(..),
	Expression(..),
	Store,
	maybe_not,
	maybe_add,
	maybe_sub,
	maybe_mult,
	maybe_expo,
	eval
)where


data Variable = Entier Int
	| Inconnue String
	deriving Show

data Expression = Var Variable
	| Neg Expression
	| Add Expression Expression
	| Sub Expression Expression
	| Mult Expression Expression
	| Expo Expression Expression
	deriving Show

type Store = [(String, Int)]

-- Fonctions de calcul avec des Maybe
maybe_not :: Num a => Maybe a -> Maybe a
maybe_not a = case a of
	Just x -> Just (0 - x)
	Nothing -> Nothing

maybe_add :: Num a => Maybe a -> Maybe a -> Maybe a
maybe_add a b = case a of
	Just x -> case b of
		Just y -> Just (x + y)
		Nothing -> Nothing
	Nothing -> Nothing
	
maybe_sub :: Num a => Maybe a -> Maybe a -> Maybe a
maybe_sub a b = case a of
	Just x -> case b of
		Just y -> Just (x - y)
		Nothing -> Nothing
	Nothing -> Nothing
	
maybe_mult :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybe_mult a b = case a of
	Just x -> case b of
		Just y -> Just (x * y)
		Nothing -> Nothing 	
	Nothing -> Nothing

maybe_expo :: (Num a, Integral b) => Maybe a -> Maybe b -> Maybe a
maybe_expo a b = case a of
	Just x -> case b of
		Just y -> Just (x ^ y)
		Nothing -> Nothing 	
	Nothing -> Nothing


-- Fonction d'évalation d'expression
eval :: Store -> Expression ->  Maybe Int
eval store exp = case exp of
	Var v -> case v of
		Entier e -> Just e
		Inconnue i -> lookup i store
	Neg e -> maybe_not (eval store e)
	Add e1 e2 -> maybe_add (eval store e1) (eval store e2)
	Sub e1 e2 -> maybe_sub (eval store e1) (eval store e2)
	Mult e1 e2 -> maybe_mult (eval store e1) (eval store e2)
	Expo e1 e2 -> maybe_expo (eval store e1) (eval store e2)
