-- Název: simplify-bkg
-- Popis: Funkcionální projekt do předmětu FLP 2021
--
-- Autor: Jan Chaloupka (xchalo16)
-- Datum: 2021-04-11

-- | Definice typů a pomocných obecných funkcí
module Common where

-- | Reprezentace neterminálů gramatiky
type Nonterm = Char
-- | Reprezentace terminálů gramatiky
type Term = Char
-- | Reprezentace terminálu nebo neterminálu gramatiky
type Symbol = Char

-- | Reprezentace jednoho pravidla gramatiky.
--   Přepis prvního neterminálu na symboly (množina terminálů a neterminálů)
data Rule = Rule Nonterm [Symbol]

-- | Interní reprezentace bezkontextové gramatiky.
--   Pořadí prvků je shodně s pořadím na vstupu - G(N, T, S, P)
data Grammar = Grammar [Nonterm] [Term] Nonterm [Rule]

-- | Sjednotit prvky dvou seznamů do jednoho seznamu (bez duplikátů)
union :: (Eq a) => [a] -> [a] -> [a]
union (x:xs) y
    | x `elem` y = union xs y
    | otherwise = x : union xs y
union [] y = y

-- | Vytvořit seznam obsahujícíc průnik dvou seznamů
intersect :: (Eq a) => [a] -> [a] -> [a]
intersect (x:xs) y
    | x `elem` y = x : intersect xs y
    | otherwise = intersect xs y
intersect [] _ = []
