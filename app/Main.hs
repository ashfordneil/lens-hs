module Main where

import Arguments

type Arg = [String]

myShortValued :: String -> Maybe (Arg -> String -> Result Arg)
myShortValued "f" = Just (const . Right)
myShortValued _ = Nothing

myLongValued :: String -> Maybe (Arg -> String -> Result Arg)
myLongValued "foo" = Just (const . Right)
myLongValued "frobnicate" = Just (const . Right)
myLongValued _ = Nothing

myShort :: String -> Maybe (Arg -> Result Arg)
myShort "a" = Just Right
myShort "b" = Just Right
myShort _ = Nothing

myLong :: String -> Maybe (Arg -> Result Arg)
myLong "awesome" = Just Right
myLong "brief" = Just Right
myLong _ = Nothing

myPositional :: Maybe (Arg -> [String] -> Result Arg)
myPositional = Just (\_ arg -> Right arg)

myStructure = ArgStructure { short=myShort, long=myLong, shortValued=myShortValued, longValued=myLongValued, positional=myPositional }

main :: IO ()
main = do
  args <- fromArgs myStructure []
  putStrLn $ show args
