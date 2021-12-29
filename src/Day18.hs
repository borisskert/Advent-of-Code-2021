module Day18 where

import Data.Char (digitToInt, isNumber)

-- https://adventofcode.com/2021/day/18

data Snailfish = Regular Int | Pair (Snailfish, Snailfish) deriving (Eq, Show)

-- -------------------------------------------------

data Action = Shift Int | Reduce String | Goto Int

data State = State Int deriving (Eq, Show)

data Symbol = T Char | N Char deriving (Eq, Show)

slrParse :: String -> Snailfish
slrParse inputText = slr [(State 0)] [] inputText []
  where
    slr :: [State] -> [Symbol] -> [Char] -> [Snailfish] -> Snailfish
    slr stateStack symbols text results = perform state stateStack symbols text results
      where
        (state : states) = stateStack
        perform :: State -> [State] -> [Symbol] -> [Char] -> [Snailfish] -> Snailfish
        perform (State 0) states symbols [] results
          | null symbols = error ("State 0: symbols missing")
          | currentSymbol == (N ('P')) = slr ((State 1) : states) symbols [] results
          | otherwise = error "State 0: Illegal state detected"
          where
            currentSymbol = head $ symbols
        perform (State 0) states symbols text results
          | c == '[' = slr ((State 2) : states) ((T '[') : symbols) cs results
          | null symbols = error ("State 0: symbols missing")
          | (== (N 'P')) . head $ symbols = slr ((State 1) : states) symbols text results
          | otherwise = error "???"
          where
            (c : cs) = text
        perform (State 1) states symbols [] results = head results
        perform (State 2) states symbols text results
          | c == '[' = slr ((State 2) : states) ((T '[') : symbols) cs results
          | isNumber c = slr ((State 7) : states) ((T c) : symbols) cs results
          | currentSymbol == (N ('P')) = slr ((State 5) : states) symbols text results
          | currentSymbol == (N ('N')) = slr ((State 4) : states) symbols text results
          | currentSymbol == (N ('R')) = slr ((State 3) : states) symbols text results
          | otherwise = error ("State 2: Illegal state detected: " ++ [c] ++ " -- " ++ show currentSymbol)
          where
            (c : cs) = text
            currentSymbol = head $ symbols
        perform (State 3) (_ : states) (_ : symbols) [] results = slr states ((N 'N') : symbols) [] results
        perform (State 3) states symbols text results
          | (c == ']' || c == ',') && r == (N 'R') = slr newStates ((N 'N') : otherSymbols) text results
          | otherwise = error "State 3: Unexpected symbol TODO or state TODO"
          where
            (c : cs) = text
            (r : otherSymbols) = symbols
            (_ : newStates) = states
        perform (State 4) states symbols (c : cs) results
          | c == ',' = slr ((State 6) : states) ((T ',') : symbols) cs results
          | otherwise = error ("State 4: " ++ unexpectedSymbol text)
        perform (State 5) (_ : states) (_ : symbols) [] results = slr states ((N 'N') : symbols) [] results
        perform (State 5) states symbols text results
          | c == ']' || c == ',' && p == 'P' = slr newStates ((N 'N') : otherSymbols) text results
          | otherwise = error "State 5: Unexpected symbol and state"
          where
            (c : cs) = text
            ((N p) : otherSymbols) = symbols
            (_ : newStates) = states
        perform (State 6) states symbols text results
          | c == '[' = slr (State 2 : states) ((T '[') : symbols) cs results
          | isNumber c = slr (State 7 : states) ((T c) : symbols) cs results
          | currentSymbol == (N ('P')) = slr ((State 5) : states) symbols text results
          | currentSymbol == (N ('N')) = slr ((State 9) : states) symbols text results
          | currentSymbol == (N ('R')) = slr ((State 3) : states) symbols text results
          | otherwise = error ("State 6: " ++ unexpectedSymbol text ++ " - or - " ++ "Unexpected state TODO")
          where
            (c : cs) = text
            currentSymbol = head $ symbols
        perform (State 7) (_ : states) symbols [] results = slr states ((N 'R') : otherSymbols) text (Regular (digitToInt i) : results)
          where
            ((T i) : otherSymbols) = symbols
        perform (State 7) states symbols text results
          | c == ']' || c == ',' = slr newStates ((N 'R') : otherSymbols) text (Regular (digitToInt i) : results)
          | otherwise = error ("State 7: " ++ unexpectedSymbol text)
          where
            (c : cs) = text
            ((T i) : otherSymbols) = symbols
            (_ : newStates) = states
        perform (State 9) states symbols text results
          | c == ']' = slr ((State 10) : states) ((T ']') : symbols) cs results
          | otherwise = error ("State 9: " ++ unexpectedSymbol text)
          where
            (c : cs) = text
        perform (State 10) (_ : _ : _ : _ : _ : states) (_ : _ : _ : _ : _ : symbols) [] results = slr states ((N 'P') : symbols) text (Pair (r1, r2) : otherResults)
          where
            (r2 : r1 : otherResults) = results
        perform (State 10) states symbols text results
          | (c == ']' || c == ',') && bo == '[' && n1 == 'N' && comma == ',' && n2 == 'N' && bc == ']' =
            slr newStates ((N 'P') : otherSymbols) text (Pair (r1, r2) : otherResults)
          | otherwise = error ("Illegal state found on stack (State 10): " ++ [c] ++ " " ++ (bo : n1 : comma : n2 : bc : []))
          where
            (c : cs) = text
            ((T bc) : (N n1) : (T comma) : (N n2) : (T bo) : otherSymbols) = symbols
            (_ : _ : _ : _ : _ : newStates) = states
            (r2 : r1 : otherResults) = results
        perform state _ _ _ _ = error ("Unknown state " ++ show state)

        unexpectedSymbol :: String -> String
        unexpectedSymbol text = "Unexpected symbol '" ++ [head text] ++ "' at index " ++ show (length inputText - length text)

parseInput :: String -> [Snailfish]
parseInput = map slrParse . lines

toString :: Snailfish -> String
toString (Regular i) = show i
toString (Pair (a, b)) = "[" ++ left ++ "," ++ right ++ "]"
  where
    left = toString a
    right = toString b

reduce :: Snailfish -> Snailfish
reduce x
  | x /= exploded = reduce exploded
  | x /= splitted = reduce splitted
  | otherwise = x
  where
    exploded = explode x
    splitted = split x

explode :: Snailfish -> Snailfish
-- (((((a,b),c),d),e),f) -- LLLL
explode (Pair (Pair (Pair (Pair (Pair (Regular a, b), c), d), e), f)) = Pair (Pair (Pair (Pair (Regular 0, merge b c), d), e), f)
-- ((((c,(a,b)),d),e),f) -- LLLR
explode (Pair (Pair (Pair (Pair (c, Pair (a, b)), d), e), f)) = Pair (Pair (Pair (Pair (merge c a, Regular 0), merge b d), e), f)
-- (((d,((a,b),c)),e),f)  -- LLRL
explode (Pair (Pair (Pair (d, Pair (Pair (a, b), c)), e), f)) = Pair (Pair (Pair (merge d a, Pair (Regular 0, merge b c)), e), f)
-- (((d,(c,(a,b))),e),f)  -- LLRR
explode (Pair (Pair (Pair (d, Pair (c, Pair (a, b))), e), f)) = Pair (Pair (Pair (d, Pair (merge c a, Regular 0)), merge b e), f)
-- ((e,(((a,b),c),d)),f) -- LRLL
explode (Pair (Pair (e, Pair (Pair (Pair (a, b), c), d)), f)) = Pair (Pair (merge e a, Pair (Pair (Regular 0, merge b c), d)), f)
-- ((e,((c,(a,b)),d)),f) -- LRLR
explode (Pair (Pair (e, Pair (Pair (c, Pair (a, b)), d)), f)) = Pair (Pair (e, Pair (Pair (merge c a, Regular 0), merge b d)), f)
-- ((e,(d,((a,b),c))),f)  -- LRRL
explode (Pair (Pair (e, Pair (d, Pair (Pair (a, b), c))), f)) = Pair (Pair (e, Pair (merge d a, Pair (Regular 0, merge b c))), f)
-- ((e,(d,(c,(a,b)))),f) -- LRRR
explode (Pair (Pair (e, Pair (d, Pair (c, Pair (a, b)))), f)) = Pair (Pair (e, Pair (d, Pair (merge c a, Regular 0))), merge b f)
-- (f,((((a,b),c),d),e)) -- RLLL
explode (Pair (f, Pair (Pair (Pair (Pair (a, b), c), d), e))) = Pair (merge f a, Pair (Pair (Pair (Regular 0, merge b c), d), e))
-- (f,(((c,(a,b)),d),e)) -- RLLR
explode (Pair (f, Pair (Pair (Pair (c, Pair (a, b)), d), e))) = Pair (f, Pair (Pair (Pair (merge c a, Regular 0), merge b d), e))
-- (f,((d,((a,b),c)),e)) -- RLRL
explode (Pair (f, Pair (Pair (d, Pair (Pair (a, b), c)), e))) = Pair (f, Pair (Pair (merge d a, Pair (Regular 0, merge b c)), e))
-- (f,((d,(c,(a,b))),e)) -- RLRR
explode (Pair (f, Pair (Pair (d, Pair (c, Pair (a, b))), e))) = Pair (f, Pair (Pair (d, Pair (merge c a, Regular 0)), merge b e))
-- (f,(e,(((a,b),c),d))) -- RRLL
explode (Pair (f, Pair (e, Pair (Pair (Pair (a, b), c), d)))) = Pair (f, Pair (merge e a, Pair (Pair (Regular 0, merge b c), d)))
-- (f,(e,((c,(a,b)),d))) -- RRLR
explode (Pair (f, Pair (e, Pair (Pair (c, Pair (a, b)), d)))) = Pair (f, Pair (e, Pair (Pair (merge c a, Regular 0), merge b d)))
-- (f,(e,(d,((a,b),c)))) --RRRL
explode (Pair (f, Pair (e, Pair (d, Pair (Pair (a, b), c))))) = Pair (f, Pair (e, Pair (merge d a, Pair (Regular 0, merge b c))))
-- (f,(e,(d,(c,(a,b))))) -- RRRR
explode (Pair (f, Pair (e, Pair (d, Pair (c, Pair (a, Regular b)))))) = Pair (f, Pair (e, Pair (d, Pair (merge c a, Regular 0))))
explode x = x

merge :: Snailfish -> Snailfish -> Snailfish
merge (Regular a) (Regular b) = Regular (a + b)
merge (Regular a) (Pair (b, c)) = Pair (merge (Regular a) b, c)
merge (Pair (a, b)) (Regular c) = Pair (a, merge b . Regular $ c)
merge (Pair (a, b)) (Pair (c, d)) = Pair (a, Pair (merge b c, d))

split :: Snailfish -> Snailfish
split (Regular a)
  | a > 9 = Pair (Regular left, Regular right)
  | otherwise = Regular a
  where
    left = a `div` 2
    right = a - left
split (Pair (a, b))
  | a /= splitA = Pair (splitA, b)
  | otherwise = Pair (a, splitB)
  where
    splitA = split a
    splitB = split b

add :: Snailfish -> Snailfish -> Snailfish
add a b = reduce $ Pair (a, b)

snailfishSum :: String -> Snailfish
snailfishSum input = foldl add (head snailfishs) . tail $ snailfishs
  where
    snailfishs = parseInput input :: [Snailfish]

magnitude :: Snailfish -> Int
magnitude (Regular x) = x
magnitude (Pair (a, b)) = 3 * magnitudeA + 2 * magnitudeB
  where
    magnitudeA = magnitude a
    magnitudeB = magnitude b

finalSnailfishSum :: String -> Int
finalSnailfishSum = magnitude . snailfishSum
