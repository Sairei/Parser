import Expression
import Test.HUnit


-- x = 5
-- var = 3
store :: Store
store = [ ("x",5), ("var", 12) ]


-- 5 + 2
exp1 = 	Add 
			(Var (Entier 5))
			(Var (Entier 2))
test1 = TestCase (assertEqual "5 + 2" (Just 7) (eval store exp1))


-- 5 * 5
exp2 = 	Mult
			(Var (Entier 5))
			(Var (Entier 5))
test2 = TestCase (assertEqual "5*5" (Just 25) (eval store exp2))


-- 2 ^ 4
exp3 = 	Expo
			(Var (Entier 2))
			(Var (Entier 4))
test3 = TestCase (assertEqual "2^4" (Just 16) (eval store exp3))


-- (5*3^2) + (10*2)
exp4 = 	Add
			(Mult
				(Var (Entier 5))
				(Expo
					(Var (Entier 3))
					(Var (Entier 2))
				)
			)
			(Mult
				(Var (Entier 10))
				(Var (Entier 2))
			)
test4 = TestCase (assertEqual "(5*3^2) + (10*2)" (Just 65) (eval store exp4))

				
-- 5 * 6 + (5^(2+2)) 
exp5 = Add 
			(Mult 
				(Var (Entier 5)) 
				(Var (Entier 6))
			) 
			(Expo 
				(Var (Entier 5)) 
				(Add
					(Var (Entier 2))
					(Var (Entier 2))
				)
			)
test5 = TestCase (assertEqual "5 * 6 + (5^(2+2))" (Just 55) (eval store exp5))


-- x * 5
exp6 = 	Mult
			(Var (Inconnue "x"))
			(Var (Entier 5))
test6 = TestCase (assertEqual "x * 5" (Just 25) (eval store exp6))


-- y * 5
exp7 = 	Mult
			(Var (Inconnue "y"))
			(Var (Entier 5))
test7 = TestCase (assertEqual "y * 5" (Nothing) (eval store exp7))


-- x ^ var
exp8 = 	Expo
			(Var (Inconnue "x"))
			(Var (Inconnue "var"))
test8 = TestCase (assertEqual "x^(var)" (Just 125) (eval store exp8))


-- 
exp9 = 	Mult
			(Var (Inconnue "x"))
			(Var (Entier 5))
test9 = TestCase (assertEqual "5*5" (Just 25) (eval store exp6))
