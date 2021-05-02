{-# LANGUAGE ScopedTypeVariables #-}

import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts
  {optCommand :: !Command}

data Command
  = Sum Int Int
  | Sub Int Int

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc <> progDesc "simple calculator"
        <> header
          "haskell-calculator - a small program for calculations"
    )

versionOption :: Parser (a -> a)
versionOption = infoOption "haskell-calculator v.0.1.0.0" (long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions =
  Opts <$> hsubparser (sumCommand <> subCommand)

sumCommand :: Mod CommandFields Command
sumCommand =
  command
    "sum"
    (info sumOptions (progDesc "Sum of numbers"))

sumOptions :: Parser Command
sumOptions =
  Sum
    <$> argument auto (metavar "x" <> help "first argument")
    <*> argument auto (metavar "y" <> help "second argument")

subCommand :: Mod CommandFields Command
subCommand =
  command
    "sub"
    (info subOptions (progDesc "Subs of numbers"))

subOptions :: Parser Command
subOptions =
  Sub
    <$> argument auto (metavar "x" <> help "first argument")
    <*> argument auto (metavar "y" <> help "second argument")

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  case optCommand opts of
    Sum x y -> print $ x + y
    Sub x y -> print $ x - y
