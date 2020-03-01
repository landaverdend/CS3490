main :: IO()
main = do
	putStrLn "Enter integer value: "
	line <- getLine
	let x = (read line :: Integer)
	if (x < 0) then print ("Nonnegative inputs only")
	else print (fact x)

fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact (x - 1)
