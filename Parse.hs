module Parse where

import Common
import Data.Char
import System.Exit

parseFromStr :: [Char] -> Grammar
parseFromStr input
    | validRules rules terms nonterms && first `elem` nonterms = g
    | otherwise = error "Input grammar parsed, but it's invalid"
    where g@(Grammar nonterms terms first rules) = parseNonterm input

parseNonterm :: [Char] -> Grammar
parseNonterm (x:',':xs)
    | x `elem` nonterms = error ("duplicated nonterm " ++ [x])
    | otherwise = Grammar (charToNonTerm x : nonterms) terms first rules
    where Grammar nonterms terms first rules = parseNonterm xs

parseNonterm (x:'\n':xs) =
    Grammar [charToNonTerm x] terms first rules
    where (terms, first, rules) = parseTerm xs
parseNonterm s = error ("unexpected sequence '" ++ s ++ "' while parsing nonterms")

parseTerm :: [Char] -> ([Term], Nonterm, [Rule])
parseTerm (x:',':xs)
    | x `elem` terms = error ("duplicated term " ++ [x])
    | otherwise = (charToTerm x : terms, first, rules)
    where (terms, first, rules) = parseTerm xs

parseTerm (x:'\n':xs) =
    ([charToTerm x], first, rule)
    where (first, rule) = parseFirst xs
parseTerm s = error ("unexpected sequence '" ++ s ++ "' while parsing terms")

parseFirst :: [Char] -> (Nonterm, [Rule])
parseFirst (x:'\n':xs) =
    (charToNonTerm x, parseRule xs)
parseFirst s = error ("unexpected sequence '" ++ s ++ "' while parsing first rule")

parseRule :: [Char] -> [Rule]
parseRule [] = []
parseRule (x:'-':'>':'#':'\n':xs) =
    Rule (charToNonTerm x) []:parseRule xs
parseRule (x:'-':'>':xs) =
    Rule (charToNonTerm x) right:parseRule next
    where
        (right, next) = parseRuleRight xs

parseRule s = error ("unexpected sequence '" ++ s ++ "' while parsing rules")

parseRuleRight :: [Char] -> ([Nonterm], [Symbol])
parseRuleRight ('\n':xs) = ([], xs)
parseRuleRight (x:xs) =
    (charToSymbol x : sym, rest)
    where
        (sym, rest) = parseRuleRight xs

parseRuleRight s = error ("unexpected sequence '" ++ s ++ "' while parsing rules")

charToTerm :: Char -> Term
charToTerm ch
    | isAsciiLower ch = ch
    | otherwise = error (ch:" is not a valid term (a-z)")

charToNonTerm :: Char -> Nonterm
charToNonTerm ch
    | isAsciiUpper ch = ch
    | otherwise = error (ch:" is not a valid nonterm (A-Z)")

charToSymbol :: Char -> Symbol
charToSymbol ch
    | isAsciiUpper ch || isAsciiLower ch = ch
    | otherwise = error (ch:" is not a valid rule symbol (a-z, A-Z)")

validRules :: [Rule] -> [Term] -> [Nonterm] -> Bool
validRules [] _ _ = True
validRules (Rule rnt s:xs) t nt =
    rnt `elem` nt && validRuleSymbol s (t ++ nt) && validRules xs t nt

validRuleSymbol :: [Symbol] -> [Symbol] -> Bool
validRuleSymbol [] _ = True
validRuleSymbol (s:xs) ref = s `elem` ref && validRuleSymbol xs ref
