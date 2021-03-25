module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit

data Flag = ListLoaded | FirstStep | SimplifyFull deriving(Show)

options :: [OptDescr Flag]
options =
    [
        Option ['i'] [] (NoArg ListLoaded) "Show loaded BKG as internal representation",
        Option ['1'] [] (NoArg FirstStep) "Print BKG after first step of a simplify algorithm",
        Option ['2'] [] (NoArg SimplifyFull) "Print fully simplified BKG"
    ]

-- TODO předělat aby to vypadalo normálně
parseOpts :: [String] -> IO (Flag, String)
parseOpts argv =
    let header = "Usage: simplify-bkg [-i12] file"
    in case getOpt Permute options argv of
        ([], _, []) -> die (usageInfo header options)
        ([f], [n], []) -> return (f,n)
        (_, _, []) -> ioError (userError "Wrong number of arguments")
        (_, _, errs) -> ioError (userError (concat errs))

main = do
    argv <- getArgs
    opts <- parseOpts argv
    print opts


simplifyBkg ListLoaded = print "List loaded"

simplifyBkg FirstStep = print "First step"

simplifyBkg SimplifyFull = print "simplify full"
