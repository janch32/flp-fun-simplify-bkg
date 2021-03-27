module Main where

import System.Environment
import System.Exit

-- TODO předělat aby to vypadalo normálně
--parseOpts argv =
--    let header = "Usage: simplify-bkg [-i12] file"
--    in case getOpt Permute options argv of
--        ([], _, []) -> die (usageInfo header options)
--        ([f], [n], []) -> return (f,n)
--        (_, _, []) -> ioError (userError "Wrong number of arguments")
--        (_, _, errs) -> ioError (userError (concat errs))

data Flag = Loaded | FirstStep | Full
data Opt = Opt Flag String | None

parseOpts :: [String] -> Opt
parseOpts ["-i", file] = Opt Loaded file
parseOpts ["-1", file] = Opt FirstStep file
parseOpts ["-2", file] = Opt Full file
parseOpts s = None

main = do
    argv <- getArgs
    opt <- parseOpts argv
    simplifyBkg opt

simplifyBkg :: Opt -> IO ()
simplifyBkg (Opt Loaded file) = print "print loaded"
simplifyBkg (Opt FirstStep file) = print "first step"
simplifyBkg (Opt Full file) = print "full"
