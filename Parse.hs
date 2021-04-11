-- Název: simplify-bkg
-- Popis: Funkcionální projekt do předmětu FLP 2021
--
-- Autor: Jan Chaloupka (xchalo16)
-- Datum: 2021-04-11

-- | Načtení vstupní gramatiky do interní reprezentace včetně kontroly správnosti
module Parse where

import Common
import Data.Char

-- | Převod vstupní gramatiky v textové podobě do interní reprezentace.
--   Vstupní řetězec je načítán po znacích. Jednotlivé části vstupu používají
--   k načtení specifické funkce níže v tomto souboru. Nakonec je zkontrolována
--   sémantická správnost načtené gramatiky.
parseFromStr :: String -> Grammar
parseFromStr input
    | validRules rules terms nonterms && first `elem` nonterms = g
    | otherwise = error "Input grammar parsed, but it's invalid"
    where g@(Grammar nonterms terms first rules) = parseNonterm input

-- | Načtení neterminálů z prvního řádku s neterminály oddělenými čárkou.
parseNonterm :: String -> Grammar
parseNonterm (x:',':xs)
    | x `elem` nonterms = error ("duplicated nonterm " ++ [x])
    | otherwise = Grammar (charToNonTerm x : nonterms) terms first rules
    where Grammar nonterms terms first rules = parseNonterm xs

parseNonterm (x:'\n':xs) =
    Grammar [charToNonTerm x] terms first rules
    where (terms, first, rules) = parseTerm xs
parseNonterm s = error ("unexpected sequence '" ++ s ++ "' while parsing nonterms")

-- | Načtení terminálů z druhého řádku s terminály oddělenými čárkou.
parseTerm :: String -> ([Term], Nonterm, [Rule])
parseTerm ('\n':xs) = -- nejsou definované žádné terminály
    ([], first, rule)
    where (first, rule) = parseFirst xs

parseTerm (x:',':xs)
    | x `elem` terms = error ("duplicated term " ++ [x])
    | otherwise = (charToTerm x : terms, first, rules)
    where (terms, first, rules) = parseTerm xs

parseTerm (x:'\n':xs) =
    ([charToTerm x], first, rule)
    where (first, rule) = parseFirst xs
parseTerm s = error ("unexpected sequence '" ++ s ++ "' while parsing terms")

-- | Zjištění vstupního neterminálu
parseFirst :: String -> (Nonterm, [Rule])
parseFirst (x:'\n':xs) =
    (charToNonTerm x, parseRule xs)
parseFirst s = error ("unexpected sequence '" ++ s ++ "' while parsing first rule")

-- | Načtení pravidel gramatiky
parseRule :: String -> [Rule]
parseRule [] = []
parseRule (x:'-':'>':'#':'\n':xs) = -- speciální případ epsilon přechodu
    Rule (charToNonTerm x) []:parseRule xs
parseRule (x:'-':'>':xs) =
    Rule (charToNonTerm x) right:parseRule next
    where (right, next) = parseRuleRight xs

parseRule s = error ("unexpected sequence '" ++ s ++ "' while parsing rules")

-- | Načtení symbolů pravé strany pravidla (kromě případu, kdy jde o epsilon přechod)
parseRuleRight :: String -> ([Nonterm], [Symbol])
parseRuleRight (x:'\n':xs) = ([charToSymbol x], xs)
parseRuleRight (x:xs) =
    (charToSymbol x : sym, rest)
    where (sym, rest) = parseRuleRight xs

parseRuleRight s = error ("unexpected sequence '" ++ s ++ "' while parsing rules")

-- | Převod znaku na terminál včetně kontroly hodnoty
charToTerm :: Char -> Term
charToTerm ch
    | isAsciiLower ch = ch
    | otherwise = error (ch:" is not a valid term (a-z)")

-- | Převod znaku na neterminál včetně kontroly hodnoty
charToNonTerm :: Char -> Nonterm
charToNonTerm ch
    | isAsciiUpper ch = ch
    | otherwise = error (ch:" is not a valid nonterm (A-Z)")

-- | Převod znaku na symbol gramatiky (terminál nebo neterminál) včetně kontroly hodnoty
charToSymbol :: Char -> Symbol
charToSymbol ch
    | isAsciiUpper ch || isAsciiLower ch = ch
    | otherwise = error (ch:" is not a valid rule symbol (a-z, A-Z)")

-- | Kontrola, zda jsou všechna pravidla sémanticky v pořádku - obsahují pouze definované
--   terminály a neterminály nebo eps přechod
validRules :: [Rule] -> [Term] -> [Nonterm] -> Bool
validRules [] _ _ = True
validRules (Rule left right:xs) t nt =
    left `elem` nt && validRuleSymbol right (t ++ nt) && validRules xs t nt

-- | Kontrola pravé strany pravidla, zda obsahuje pouze povolené terminály a neterminály
validRuleSymbol :: [Symbol] -> [Symbol] -> Bool
validRuleSymbol [] _ = True
validRuleSymbol (s:xs) ref = s `elem` ref && validRuleSymbol xs ref
