-- Název: simplify-bkg
-- Popis: Funkcionální projekt do předmětu FLP 2021
--
-- Autor: Jan Chaloupka (xchalo16)
-- Datum: 2021-04-11

-- | Modul s funkcemi pro zjednodušení gramatiky - odstranění nepotřebných částí
--   při zachování ekvivalence se vstupní gramatikou
module Simplify where

import Common

-- | Provedení prvního kroku algoritmu (skripta TIN - algoritmus 4.3)
firstStep :: Grammar -> Grammar
firstStep g@(Grammar n t s p) =
    Grammar (tnt `union` [s]) t s (filterRules p tnt (t++tnt))
    where tnt = terminalNonterms g []

-- | Výpočet množiny neterminálů generující terminální řetězce.
--   Odpovídá algoritmu 4.1 v TIN skriptech.
terminalNonterms :: Grammar -> [Nonterm] -> [Nonterm]
terminalNonterms g@(Grammar n t s p) prev
    | prev == curr = curr -- nic se nezměnilo, výstup je ustálený, končíme
    | otherwise = terminalNonterms g curr
    where curr = filterRulesRight p (t++prev)

-- | Získat neterminály z levé strany konečných pravidel
filterRulesRight :: [Rule] -> [Symbol] -> [Nonterm]
filterRulesRight (Rule left right:xs) s
    | all (`elem` s) right = [left] `union` filterRulesRight xs s
    | otherwise = filterRulesRight xs s
filterRulesRight [] _ = []

-- | Filtrovat pravidla podle omezených neterminálů levé strany a symbolů pravé
filterRules :: [Rule] -> [Nonterm] -> [Symbol] -> [Rule]
filterRules (r@(Rule left right):xs) nt s
    | left `elem` nt && all (`elem` s) right = r : filterRules xs nt s
    | otherwise = filterRules xs nt s
filterRules [] _ _ = []


-- | Provedení prvního a druhého kroku algoritmu 4.3 z TIN skript pro odstranění
--   zbytečných symbolů a pravidel.
simplifyFull :: Grammar -> Grammar
simplifyFull g@(Grammar n t s p) =
    Grammar vn vt s (filterRules g1p vn (vt++vn))
    where
        g1@(Grammar _ _ _ g1p) = firstStep g -- provést první krok
        (vn, vt) = removeUnreachable g1 [s]

-- | Výpočet množiny V dostupných symbolů (algoritmus 4.2)
--   Funkce vrátí redukovanou množinu neterminálů a terminálů
removeUnreachable :: Grammar -> [Nonterm] -> ([Nonterm], [Term])
removeUnreachable g@(Grammar n t s p) prev
    | prev == curr = (curr, rulesRightSymbols prevRules t) -- výstup je ustálený, konec
    | otherwise = removeUnreachable g curr
    where
        prevRules = filterRules p prev (n++t)
        curr = prev `union` rulesRightSymbols prevRules n

-- | Získání neterminálů, které se vyskytují na pravé straně pravidel.
--   Součást algoritmu 4.2 (removeUnreachable)
rulesRightSymbols :: [Rule] -> [Symbol] -> [Symbol]
rulesRightSymbols (Rule _ right : xs) s =
    (right `intersect` s) `union` rulesRightSymbols xs s
rulesRightSymbols [] _ = []
