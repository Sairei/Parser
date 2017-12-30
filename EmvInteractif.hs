import Data.Maybe
import Data.String
import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Either
import Expression
import Parse


-- Type pour les fonctions --
type Handler = [String] -> Store -> IO()


-- Fonction qui verifie si l'input est une commande
-- C'est à dire si l'entrée commence par ":"
isCommand :: String -> Bool
isCommand (x:xs) = (x == ':')
isCommand [] = False


-- Fonstion HELP --
help :: Handler
help _ str = do
	putStrLn "Liste des commandes valides\n"
	putStrLn "h (ou 'help')"
	putStrLn "\tAffiche les commande possible"
	putStrLn "q (ou 'quit')"
	putStrLn "\tQuitte la boucle"
	putStrLn "store"
	putStrLn "\tAffiche le contenu du store"
	putStrLn ""
	putStrLn ""
	putStrLn ""
	putStr "--Tapez sur ENTRER pour sortir--"
	getLine
	mainLoop [] str
	
	
-- Fonction pour quitter le programme --
exit :: Handler
exit _ str = do
	putStrLn "Vous allez quitter le programme"
	putStrLn "Etes vous sur de vouloir quitter ? o/n"
	c <- getChar
	if c == 'n' then
		mainLoop [] str
	else
		if c /= 'o' then
			do
				putStrLn "\n"
				putStrLn "Je n'ai pas compris"
				exit [] str
		else
			putStrLn "\nFin du programme"
	
	
-- Affiche le Store courant --
affStore :: Handler
affStore _ str = do
	putStrLn ""
	putStrLn "Liste des variables rentré :"
	affStore_rec [] str
	mainLoop [] str

affStore_rec :: Handler
affStore_rec _ [] = putStrLn []
affStore_rec _ ((x,v) : xs) = do
	putStr "   =>   "
	putStr x
	putStr " = "
	putStrLn (show v)
	affStore_rec [] xs
	

-- Ajout de valeur dans le Store --	
addStore :: Handler
addStore input str = do
	putStrLn ""
	putStr "Ajout de la variable \""
	putStr (head input)
	putStr "\" avec pour valeur \""
	putStr (head (tail input))
	putStrLn "\""
	mainLoop [] (((head input), (read (head (tail input)))) : str)


-- Supprime une valeur du Store --
delStore :: Handler
delStore input str = do
	putStrLn ""
	putStr "Suppression de la variable \""
	putStr (head input)
	putStrLn "\""
	mainLoop [] (delElem input str)

delElem :: [String] -> Store -> Store
delElem _ [] = []
delElem input ((x,v):xs) = if (head input) == x 
	then delElem input xs
	else ((x,v): delElem input xs)
	

-- Fonction main --
-- main 		=> fonction main de depart
-- mainLoop 	=> main boucle (test que les input simple) 
-- test_autres	=> fait les autres test ("set" et "unset")
main :: IO ()
main = do
	putStrLn ""
	putStrLn "[Lancement du programme]"
	putStrLn "   [Modules chargés]    "
	putStrLn ""
	mainLoop [] []
	
	
mainLoop :: Handler
mainLoop _ str = do
	putStrLn "\nEntrez une commande :"	
	input <- getLine
	if (isCommand input) == True 
	then 
		do
			case input of
				":help" -> help [] str
				":h" -> help [] str
				":quit" -> exit [] str
				":q" -> exit [] str
				":store" -> affStore [] str
				otherwise -> test_autres (words input) str
	else do 
		let tmp = isExp input in
			if isJust tmp 
				then let res = eval str (fromJust tmp) in
					if isJust res
						then do
							putStrLn (show (fromJust res))
							mainLoop [] str
						else do
							putStrLn "Impossible à evaluer, :store pour voir le store"
							mainLoop [] str	
				else do
					putStrLn "Ni une commande ni une expression\n"
					mainLoop [] str
			
test_autres :: Handler
test_autres input str = do
	case head input of
		":set" -> addStore (tail input) str
		":unset" -> delStore (tail input) str
		otherwise -> do
			putStrLn "Mauvaise commande\nTapez :h (:help), pour plus d'information"
			mainLoop [] str
