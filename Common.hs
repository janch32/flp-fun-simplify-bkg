module Common where

type Nonterm = Char
type Term = Char
type Symbol = Char

data Rule = Rule Nonterm [Symbol]

data Grammar = Grammar [Nonterm] [Term] Nonterm [Rule]

union :: (Eq a) => [a] -> [a] -> [a]
union (x:xs) y
    | x `elem` y = union xs y
    | otherwise = x : union xs y
union [] y = y

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect (x:xs) y
    | x `elem` y = x : intersect xs y
    | otherwise = intersect xs y
intersect [] _ = []
