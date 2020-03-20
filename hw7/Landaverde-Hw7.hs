data Prop a = Var a | Const Bool   | And (Prop a) (Prop a) | Or (Prop a) (Prop a)
                    | Neg (Prop a) | Imp (Prop a) (Prop a) | Iff (Prop a) (Prop a)
					| Xor (Prop a) (Prop a)
  deriving (Show,Read)

data Token = VSym String | CSym Bool | BOp String | UOp | LParen | RParen | Err
             | TrueProp (Prop String)
  deriving (Show,Read)

-- LEXER
checkLetter :: Char -> Bool
checkLetter x = ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z')


--put spaces between everything.
preproc :: String -> String
preproc "" = ""
preproc ('!':xs) = " ! " ++ preproc xs
preproc ('/':('\\':xs)) = " /\\ " ++ preproc xs
preproc ('\\':('/':xs)) = " \\/ " ++ preproc xs
preproc ('-':('>':xs)) = " -> " ++ preproc xs
preproc ('<' :('>':xs)) = " <> " ++ preproc xs --Xor 
preproc ('<':('-':('>':xs))) = " <-> " ++ preproc xs
preproc ('(':xs) = " ( " ++ preproc xs
preproc (')':xs) = " ) " ++ preproc xs
preproc (x:xs) = x : preproc xs

--helper function for lexer
readOne :: String -> Token
readOne "!" = UOp
readOne "T" = CSym True
readOne "F" = CSym False
readOne "/\\" = BOp "And"
readOne "\\/" = BOp "Or"
readOne "->" = BOp "Imp"
readOne "<->" = BOp "Iff"
readOne "><" = BOp "Xor" --Xor
readOne "(" = LParen
readOne ")" = RParen
readOne (x:xs) = if checkLetter x &&
                      all (\c -> checkLetter c || ('0' <= c && c <= '9')) xs
                    then VSym (x:xs)
                    else Err
readOne _ = Err

--Convert string to token
lexer :: String -> [Token]
lexer s = map readOne (words (preproc s))


-- PARSER

parser :: [Token] -> Maybe (Prop String)
parser l = let (_,y) = helper (l,[]) in
             case y of
               [TrueProp p] -> Just p
               [Err]        -> Nothing    -- Lexical error
               _            -> Nothing    -- Parse error


helper :: ([Token],[Token]) -> ([Token],[Token])
helper ((t:ts),[])                = helper (ts,[t])
helper (_,  (Err:_))              = ([],[Err])
helper (ts, (VSym v):s)           = helper (ts,(TrueProp (Var v) : s))
helper (ts, (CSym b):s)           = helper (ts,(TrueProp (Const b) : s))
helper (ts, (TrueProp p):(UOp:s)) = helper (ts,(TrueProp (Neg p) : s))
helper (ts, (TrueProp p1 : (BOp op : (TrueProp p2 : s))))
                    | op == "And" = helper (ts,(TrueProp (And p2 p1) : s))
                    | op == "Or"  = helper (ts,(TrueProp (Or p2 p1) : s))
                    | op == "Imp" = helper (ts,(TrueProp (Imp p2 p1) : s))
                    | op == "Iff" = helper (ts,(TrueProp (Iff p2 p1) : s))
					| op == "Xor" = helper (ts,(TrueProp (Xor p2 p1) : s)) -- Xor
helper (ts, (RParen:(TrueProp p:(LParen:s)))) = helper (ts,TrueProp p : s)
helper (t:ts, s) = helper (ts,t:s)
helper ([],s) = ([],s)


-- SOLVER

removeDups :: (Eq a) => [a] -> [a]
removeDups = foldr (\x y -> x : filter (/= x) y) []

--take the Prop and turn it into a list of vars.
fv :: (Eq a) => Prop a -> [a]
fv (Var x) = [x]
fv (Const _) = []
fv (And p1 p2) = removeDups (fv p1 ++ fv p2)
fv (Or p1 p2)  = removeDups (fv p1 ++ fv p2)
fv (Imp p1 p2) = removeDups (fv p1 ++ fv p2)
fv (Iff p1 p2) = removeDups (fv p1 ++ fv p2)
fv (Xor p1 p2) = removeDups (fv p1 ++ fv p2)
fv (Neg p)     = fv p

--find value based off of tuple
lookUp :: (Eq a) => a -> [(a,Bool)] -> Bool
lookUp key = foldr (\(x1,x2) acc -> if x1 == key then x2 else acc) False

eval :: (Eq a) => [(a,Bool)] -> Prop a -> Bool
eval env (Var x) = lookUp x env
eval env (Const b) = b
eval env (And p1 p2) = eval env p1 && eval env p2
eval env (Or  p1 p2) = eval env p1 || eval env p2
eval env (Neg p) = not (eval env p)
eval env (Imp p1 p2) = if eval env p1 then eval env p2 else True
eval env (Iff p1 p2) = eval env p1 == eval env p2
eval env (Xor p1 p2) = xor (eval env p1) (eval env p2)


genEnvs :: [a] -> [[(a,Bool)]]
genEnvs = foldr (\x y -> map ((x,True):) y ++ map ((x,False):) y) [[]]

checkSat :: (Eq a) => Prop a -> Bool
checkSat p = any (\env -> eval env p) (genEnvs (fv p))

--helper 
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

--helper xor
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False


--remove just?
removeJust :: Maybe (Prop a) -> Prop a
removeJust (Just x) = x

-- EXAMPLES

myprop1 = "X /\\ F"
myprop1parsed = parser (lexer myprop1)

myprop2 = "!X \\/ !F"
myprop2parsed = parser (lexer myprop2)

myprop3 = "!(X/\\Y0)->(!X\\/!Y0)"
myprop3parsed = parser (lexer myprop3)

myprop4 = "!(X/\\Y0)<->!(!X\\/!Y0)"
myprop4parsed = parser (lexer myprop4)

--Step one: preproc line, then pass string into lexer.
--Step two: parse the tokenized string.
--Step three: if its good, then print out the stuff
main = do
   putStrLn "Enter the formula to be checked: "
   line <- getLine
   let x = parser (lexer (preproc line))
   if (isNothing x) then print "Input Error."
   else if (checkSat (removeJust x)) then print "Satisfiable"
   else print "Unsatisfiable"
