module Common where

type Nonterm = Char

type Term = Char

type Symbol = Char

data Rule = Rule Nonterm [Symbol]
    deriving Show

data Grammar = Grammar [Nonterm] [Term] Nonterm [Rule]
    deriving Show


grammarToStr :: Grammar -> String
grammarToStr (Grammar nt t first rules) =
    symbolsToStr nt ++ "\n" ++ symbolsToStr t ++ ['\n', first,'\n'] ++ rulesToStr rules

symbolsToStr :: [Symbol] -> String
symbolsToStr [x] = [x]
symbolsToStr (x:xs) = x : ',' : symbolsToStr xs
symbolsToStr [] = ""

rulesToStr :: [Rule] -> String
rulesToStr (Rule nt s:xs) = nt : "->" ++ ruleSymbolsToStr s ++ "\n" ++ rulesToStr xs
rulesToStr [] = []

ruleSymbolsToStr :: [Symbol] -> String
ruleSymbolsToStr [s] = [s]
ruleSymbolsToStr (s:xs) = s : ruleSymbolsToStr xs
