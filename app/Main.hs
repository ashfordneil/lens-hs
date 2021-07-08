{-# LANGUAGE RankNTypes #-}
module Main where

import Arguments
import UserLens

import Control.Lens
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List
import Data.Vector (fromList)
import System.IO

data Configuration = Configuration {
    lenses :: [UserLens]
  , inputFile :: Maybe String
  , outputFile :: Maybe String
  } deriving Show

inputFileOpt :: ValuedOption Configuration
inputFileOpt base @ Configuration { inputFile=Nothing } fileName = Right $ Configuration {
    lenses=lenses base
  , inputFile=Just fileName
  , outputFile=outputFile base
  }
inputFileOpt _ _ = Left "Only one input file can be specified"

outputFileOpt :: ValuedOption Configuration
outputFileOpt base @ Configuration { outputFile=Nothing } fileName = Right $ Configuration {
    lenses=lenses base
  , inputFile=inputFile base
  , outputFile=Just fileName
  }
outputFileOpt _ _ = Left "Only one output file can be specified"

shortOpt :: String -> Maybe (ValuedOption Configuration)
shortOpt "i" = Just inputFileOpt
shortOpt "o" = Just outputFileOpt
shortOpt _ = Nothing

longOpt :: String -> Maybe (ValuedOption Configuration)
longOpt "input" = Just inputFileOpt
longOpt "output" = Just outputFileOpt
longOpt _ = Nothing

positionalOpt :: PositionalOptions Configuration
positionalOpt base vals = (\l -> Configuration { lenses=l, inputFile=inputFile base, outputFile=outputFile base }) <$> mapM getUserLens vals

argConfig :: ArgStructure Configuration
argConfig = ArgStructure {
    shortValued=shortOpt
  , short=const Nothing
  , longValued=longOpt
  , long=const Nothing
  , positional=Just positionalOpt
  }

readInput :: Configuration -> IO String
readInput Configuration { inputFile=Nothing } = stdin & hGetContents
readInput Configuration { inputFile=Just fname } = readFile fname

main :: IO ()
main = do
  args <- fromArgs argConfig $ Configuration { lenses=[], inputFile=Nothing, outputFile=Nothing }
  input <- readInput args
  output <- pure . Array . fromList $ input ^.. toLens (lenses args)
  B.putStrLn $ encodePretty output
