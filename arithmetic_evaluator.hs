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
              | null xs = z ++ [(y ++ [x])]
              | (checkOperator x) = calculate xs [] ((z ++ [y]) ++ [[x]])
              | otherwise = calculate xs (y ++ [x]) z
              
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
              | operator == "+" = show((read operand2) + (read operand1))
              | operator == "-" = show((read operand2) - (read operand1))
              | operator == "*" = show((read operand2) * (read operand1))
              | operator == "/" = show((read operand2) / (read operand1))
              | otherwise = "ERROR"

--Actual function the user calls
evaluate :: [Char] -> Integer
evaluate x
              | null x = -1
              | otherwise = interperatePostFix (convertToPostFix (calculate (removeWhiteSpace x) "" []) [] []) []
