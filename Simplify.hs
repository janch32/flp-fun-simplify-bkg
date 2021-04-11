module Simplify where

import Common

firstStep :: Grammar -> Grammar
firstStep g@(Grammar n t s p) =
    Grammar tnt t s (filterRules p tnt (t++tnt))
    where tnt = terminalNonterms g []

terminalNonterms :: Grammar -> [Nonterm] -> [Nonterm]
terminalNonterms g@(Grammar n t s p) prev
    | prev == curr = curr
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

simplifyFull :: Grammar -> Grammar
simplifyFull g@(Grammar n t s p) =
    Grammar vn vt s (filterRules g1p vn (vt++vn))
    where
        g1@(Grammar _ _ _ g1p) = firstStep g
        (vn, vt) = removeUnreachable g1 [s]

removeUnreachable :: Grammar -> [Nonterm] -> ([Nonterm], [Term])
removeUnreachable g@(Grammar n t s p) prev
    | prev == curr = (curr, rulesRightSymbols prevRules t)
    | otherwise = removeUnreachable g curr
    where
        prevRules = filterRules p prev (n++t)
        curr = prev `union` rulesRightSymbols prevRules n

rulesRightSymbols :: [Rule] -> [Symbol] -> [Symbol]
rulesRightSymbols (Rule _ right : xs) s =
    (right `intersect` s) `union` rulesRightSymbols xs s
rulesRightSymbols [] _ = []
