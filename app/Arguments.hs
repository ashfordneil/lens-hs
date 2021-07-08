module Arguments (ArgStructure(..), Result, ValuedOption, PlainOption, PositionalOptions, fromArgs) where

import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import System.Environment
import System.Exit

-- Utilities
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse _ (Just x) = Just x
orElse _ _ = Nothing

(<??>) :: Functor f => f (a -> b) -> a -> f b
fab <??> a = (\ab -> ab a) <$> fab

-- Exported types
type Result a = Either String a

type ValuedOption a = a -> String -> Result a
type PlainOption a = a -> Result a
type PositionalOptions a = a -> [String] -> Result a

data ArgStructure a = ArgStructure {
    shortValued :: String -> Maybe (ValuedOption a)
  , longValued :: String -> Maybe (ValuedOption a)
  , short :: String -> Maybe (PlainOption a)
  , long :: String -> Maybe (PlainOption a)
  , positional :: Maybe (PositionalOptions a)
  }

-- ArgType
data ArgType = Short String
             | Long String
             | Positional String
             | NowAllPositional

instance Show ArgType where
  show (Short x) = "-" ++ x
  show (Long x) = "--" ++ x
  show (Positional x) = x
  show NowAllPositional = "--"

argType :: String -> ArgType
argType "--" = NowAllPositional
argType arg = let
  long = Long <$> stripPrefix "--" arg
  short = Short <$> stripPrefix "-" arg
  pos = Positional arg
  shortOrPos = fromMaybe pos short
  in fromMaybe shortOrPos long

-- Parse state
data ParseState a = ParseState {
    base :: a
  , remaining :: [String]
  }

applyValued :: ArgType -> ValuedOption a -> ParseState a -> Result (ParseState a)
applyValued _ apply state @ ParseState { remaining=(arg:args) } =
  apply (base state) arg <&> (\base' -> ParseState {
      base=base'
    , remaining=args
    })
applyValued arg _ ParseState { remaining=[] } =
  Left ("Expected argument following " ++ (show arg))

applyPlain :: PlainOption a -> ParseState a -> Result (ParseState a)
applyPlain apply state = apply (base state) <&> (\base' -> ParseState {
    base=base'
  , remaining=remaining state
  })

-- parse all of the opts, until we have nothing but state and positional args
parseOpts' :: ArgStructure a -> ArgType -> ParseState a -> Result (a, [String])
parseOpts' ctx a @ (Short arg) state = let
  valued = applyValued a <$> shortValued ctx arg
  plain = applyPlain <$> short ctx arg
  stepM = orElse valued plain
  stepE = fromMaybe (Left $ "Unexpected argument " ++ show a) (Right <$> stepM)
  in stepE >>= (\step -> step state) >>= parseOpts ctx
parseOpts' ctx a @ (Long arg) state = let
  valued = applyValued a <$> longValued ctx arg
  plain = applyPlain <$> long ctx arg
  stepM = orElse valued plain
  stepE = fromMaybe (Left $ "Unexpected argument " ++ show a) (Right <$> stepM)
  in stepE >>= (\step -> step state) >>= parseOpts ctx
parseOpts' ctx (Positional pos) state =
  (\(state', positionals) -> (state', pos : positionals)) <$> parseOpts ctx state
parseOpts' ctx NowAllPositional state = Right (base state, remaining state)

parseOpts :: ArgStructure a -> ParseState a -> Result (a, [String])
parseOpts ctx ParseState { base=base, remaining=[] } = Right (base, [])
parseOpts ctx ParseState { base=base, remaining=(arg:args) } =
  parseOpts' ctx (argType arg) ParseState { base=base, remaining=args }

parse :: ArgStructure a -> a -> [String] -> Result a
parse ctx base args = do
  (optState, positionalArgs) <- parseOpts ctx ParseState { base=base, remaining=args }
  applyPositional <- case positionalArgs of {
    [] -> Right $ (const . Right);
    _ -> fromMaybe (Left "Unexpected positional argument") (Right <$> positional ctx)
  }
  applyPositional optState positionalArgs

bailOnError :: Result a -> IO a
bailOnError (Right a) = pure a
bailOnError (Left msg) = do
  putStrLn msg
  exitWith $ ExitFailure 1

fromArgs :: ArgStructure a -> a -> IO a
fromArgs ctx base = getArgs <&> parse ctx base >>= bailOnError
