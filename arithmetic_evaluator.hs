import Text.Parsec as Parsec

--Simple helped function to check for operators to distinguish operator and numbers
checkOperator :: Char -> Bool
checkOperator x
                | (x == '+') || (x == '-') || (x == '*') || (x == '/') = True
                | otherwise  = False

--Removes white space since it is unnecessary in infix expressions
removeWhiteSpace :: [Char] -> [Char]
removeWhiteSpace (x:xs)
              | xs == "" = (x : xs)
              | x == ' ' = removeWhiteSpace xs
              | otherwise = x : (removeWhiteSpace xs)


--Poorly named function, this parses the string into a list of individual expressions
calculate :: [Char] -> [Char] -> [[Char]] -> [[Char]]
calculate (x:xs) y z
              | null xs = ((x : y) : z)
              | (checkOperator x) = calculate xs [] ([x]:(y : z))
              | otherwise = calculate xs (x : y) z

--I'm not sure why I made calculate the way I did, but it writes backwards.
--I will change that immediately, because this function and the one underneath is extraneous
reorient :: [[Char]] -> [[Char]]
reorient (x:xs)
              | null xs = [(fixNum x)]
              | otherwise = (reorient xs) ++ [(fixNum x)]

fixNum :: [Char] -> [Char]
fixNum (x:xs)
              | null xs = [x]
              | otherwise = (fixNum xs) ++ (fixNum [x])

--Self explanatory
greaterOrEqualPrecedence :: [Char] -> [Char] -> Bool
greaterOrEqualPrecedence first second
              | (first == "*") || (first == "/") = True
              | ((first == "+") || (first == "-")) && ((second == "+") || (second == "-")) = True
              | otherwise  = False

--Also self explanatory
convertToPostFix :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
convertToPostFix input@(x:xs) stack result
              | null xs = emptyStack (result ++ [x]) stack
              -- | null xs = convertToPostFix input (tail stack) ((result ++ [x]) ++ [(head stack)])
              | (checkOperator (head x)) && (null stack || (greaterOrEqualPrecedence x (head stack))) = convertToPostFix xs (x : stack) result
              | (checkOperator (head x)) = convertToPostFix input (tail stack) (result ++ [head stack])
              | otherwise = convertToPostFix xs stack (result ++ [x]) --Assume it's a number

--This takes everything in the stack and attaches it to the end of the expression
emptyStack :: [[Char]] -> [[Char]] -> [[Char]]
emptyStack result stack@(s:ss)
              | null ss = result ++ [s]
              | otherwise = emptyStack (result ++ [s]) ss

--Turns the postfix expression into a number
interperatePostFix :: [[Char]] -> [[Char]] -> Integer
interperatePostFix input@(x:xs) stack
              | null xs  = read (doExpression (head stack) x (head (tail stack))) --base case
              | (checkOperator (head x)) = interperatePostFix xs ((doExpression (head stack) x (head (tail stack))) : (tail (tail stack)))
              | otherwise = interperatePostFix xs (x : stack)

--Takes three strings, does the math, then converts the result in a string
doExpression :: [Char] -> [Char] -> [Char] -> [Char]
doExpression operand1 operator operand2
              | operator == "+" = show((read operand1) + (read operand2))
              | operator == "-" = show((read operand1) - (read operand2))
              | operator == "*" = show((read operand1) * (read operand2))
              | operator == "/" = show((read operand1) / (read operand2))
              | otherwise = "ERROR"

--Actual function the user calls
evaluate :: [Char] -> Integer
evaluate x
              | null x = -1
              | otherwise = interperatePostFix (convertToPostFix (reorient (calculate (removeWhiteSpace x) "" [])) [] []) []
