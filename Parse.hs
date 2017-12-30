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
containErr (Add a b) = containErr a || containErr b
containErr (Mult a b) = containErr a || containErr b
containErr (Expo a b) = containErr a || containErr b
containErr (Var e) = case e of
	Inconnue xs -> (xs == "")
	otherwise -> False


-- Fonction pour suprimmer les espaces
noSpace :: String -> String
noSpace input = concat (words input)


-- Fonctions de construction d'expression
expr :: Parser Expression
expr = try
	(
	do
		t <- term
		e <- expB t
		return e
	)
	<|>
	do
		return (Var (Inconnue "")) 

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
term = try
	(
	do
		p <- pow
		t <- terB p
		return t
	)
	<|>
	do
		return (Var (Inconnue "")) 	

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
pow = try 
	(
	do
		n <- neg
		p <- powB n
		return p
	)
	<|>
	do
		return (Var (Inconnue "")) 
	
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
		b <- many1 letter
		return (Var (Inconnue b))
	)
	<|>
	do
		return (Var (Inconnue "")) 
	
	
-- Fonction qui retourne une maybe expression
isExp :: String -> Maybe Expression
isExp xs = 
	let 
		e = partitionEithers ((parse expr "" (noSpace xs)):[]) 
	in
		if containErr (head (snd e))
			then Nothing
			else Just (head (snd e))
