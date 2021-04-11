module Simplify where

import Common

firstStep :: Grammar -> Grammar
firstStep g@(Grammar n t s p) =
    Grammar tnt t s (filterRules p tnt (t++tnt))
    where tnt = terminalNonterms g [s]

terminalNonterms :: Grammar -> [Nonterm] -> [Nonterm]
terminalNonterms g@(Grammar n t s p) prev
    | prev == curr = prev
    | otherwise = terminalNonterms g curr
    where curr = filterRulesRight p (t++prev)

filterRulesRight :: [Rule] -> [Symbol] -> [Nonterm]
filterRulesRight (Rule left right:xs) s
    | all (`elem` s) right = left : filterRulesRight xs s
    | otherwise = filterRulesRight xs s
filterRulesRight [] _ = []

filterRules :: [Rule] -> [Nonterm] -> [Symbol] -> [Rule]
filterRules (r@(Rule left right):xs) nt s
    | left `elem` nt && all (`elem` s) right = r : filterRules xs nt s
    | otherwise = filterRules xs nt s
filterRules [] _ _ = []

simplify :: Grammar -> Grammar
simplify g = g
