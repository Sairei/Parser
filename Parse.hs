module Parse (
	noSpace, 
	expr, expB, term, terB,
	pow, powB, neg, fac,
	isExp
)where

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Data.Char
import Data.Either
import Expression

					
-- Fonction sur les listes
containErr :: Expression -> Bool
containErr exp = case exp of
	Var v -> do
		case v of
			Inconnue xs -> (xs == "")
			otherwise -> False
	ortherwise -> False
	

-- Fonction pour suprimmer les espaces
noSpace :: String -> String
noSpace input = concat (words input)


-- Fonctions de construction d'expression
expr :: Parser Expression
expr = do
	t <- term
	e <- expB t
	return e
	
expB :: Expression -> Parser Expression
expB h = try 
	(
	do
		char '+'
		t <- term
		e <- expB (Add h t)
		return e
	)
	<|>
	try (
	do
		char '-'
		t <- term
		e <- expB (Sub h t)
		return e
	)
	<|>
	try (
	do
		return h
	)
	<|>
	do
		return (Var (Inconnue "")) 
	
term :: Parser Expression
term = do
	p <- pow
	t <- terB p
	return t
	
terB :: Expression -> Parser Expression
terB h = try
	(
	do
		char '*'
		p <- pow
		t <- terB (Mult h p)
		return t
	)
	<|>
	try (
	do
		return h
	)
	<|>
	do
		return (Var (Inconnue "")) 
	
pow :: Parser Expression
pow = do
	n <- neg
	p <- powB n
	return p
	
powB :: Expression -> Parser Expression
powB h = try
	(
	do
		char '^'
		n <- neg
		p <- powB (Expo h n)
		return p
	)
	<|>
	try (
	do
		return h
	)
	<|>
	do
		return (Var (Inconnue "")) 
	
neg :: Parser Expression
neg = try
	(
	do
		char '-'
		e <- expr
		return (Neg e)
	)
	<|>
	try (
	do
		f <- fac
		return f
	)
	<|>
	do
		return (Var (Inconnue "")) 

fac :: Parser Expression
fac = try
	(
	do
		char '('
		e <- expr
		char ')'
		return e
	)
	<|>
	try (
	do
		b <- many1 digit
		return (Var (Entier (read (b)::Int)))
	)
	<|>
	try (
	do
		b <- many letter
		return (Var (Inconnue b))
	)
	
	
-- Fonction qui retourne une maybe expression
isExp :: String -> Maybe Expression
isExp xs = 
	let 
		e = partitionEithers ((parse expr "" (noSpace xs)):[]) 
	in
		if containErr (head (snd e))
			then Just (head (snd e))
			else Nothing
