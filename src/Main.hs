{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import Control.Exception (evaluate)
import           Control.Monad (forM_, replicateM, join)
import           Control.Monad.State (execState, get, modify)
import           Data.Bool (bool)
import           Data.Monoid ((<>))
import           Data.String.Conv (toS)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment
import           System.IO (openFile, Handle, stdout, stdin, IOMode (..), hGetContents, hPutStr, hFlush)
import           Text.LaTeX.Base.Parser (parseLaTeXWith, ParserConf (..))
import           Text.LaTeX.Base.Render (readFileTex, render)
import           Text.LaTeX.Base.Syntax (LaTeX (..), TeXArg (..), lookForCommand, texmap)
import Debug.Trace

type Latex = LaTeX

data Config = Config
  { configInput  :: T.Text
  , configOutput :: Handle
  }

parserConf :: ParserConf
parserConf = ParserConf
  { verbatimEnvironments = ["code", "verbatim", "lstlisting", "dorepl"]
  }

getFileName :: String -> Maybe String
getFileName "-" = Nothing
getFileName f   = Just f

getConfig :: IO Config
getConfig = do
  inf   <- init <$> getArgs
  outf  <- last <$> getArgs
  input <- traverse (maybe TIO.getContents TIO.readFile . getFileName) inf
  Config <$> (pure $ T.concat input)
         <*> (bool (openFile outf WriteMode) (pure stdout) $ outf == "-")


data Cmd = Cmd
  { cmdName :: String
  , cmdArgs :: Int
  , cmdCont :: [Latex] -> Latex
  }

data Env = Env
  { envName :: String
  , envArgs :: Int
  , envCont :: [Latex] -> Latex -> Latex
  }

mkCmd :: [TeXArg] -> Cmd
mkCmd [ FixArg (TeXCommS cmdName)
      , FixArg d
      ] = Cmd (cmdName) 0 $ const d
mkCmd [ FixArg (TeXCommS cmdName)
      , OptArg (TeXRaw (read . toS -> cmdArgs))
      , FixArg d
      ] = Cmd {..}
  where
    cmdCont = flip spliceLaTeX d
mkCmd a = error $ show a

mkEnv :: [TeXArg] -> Env
mkEnv [ FixArg (TeXRaw (toS -> envName))
      , FixArg o
      , FixArg c
      ] = Env (envName) 0 $ \_ d -> o <> d <> c
mkEnv [ FixArg (TeXRaw (toS -> envName))
      , OptArg (TeXRaw (read . toS -> envArgs))
      , FixArg o
      , FixArg c
      ] = Env {..}
  where
    envCont args d = spliceLaTeX args $ o <> d <> c

spliceLaTeX :: [Latex] -> Latex -> Latex
spliceLaTeX args d' = flip execState d'
                    . forM_ (zip [1..] args)
                    $ \(idx, arg) -> modify . texmap isRaw
                                            $ splice idx arg
  where
    splice :: Int -> Latex -> Latex -> Latex
    splice idx arg (TeXRaw t) = result
      where
        result = either (error . show) id . parseLaTeXWith parserConf $ T.replace (toS $ "#" <> show idx) (render arg) t

isRaw :: Latex -> Bool
isRaw (TeXRaw _) = True
isRaw _          = False

matchName :: String -> Latex -> Bool
matchName s (TeXComm s' _) = s == s'
matchName s (TeXCommS s' ) = s == s'
matchName _ _              = False

matchEnvName :: String -> Latex -> Bool
matchEnvName s (TeXEnv s' _ _) = s == s'
matchEnvName _ _               = False

runEnv :: Env -> Latex -> Latex
runEnv env = texmap (matchEnvName $ envName env) $ \(TeXEnv _ args d) ->
    envCont env (fmap fixArg args) d

runCmd :: Cmd -> Latex -> Latex
runCmd cmd = texmap (matchName $ cmdName cmd) $ \d ->
    cmdCont cmd . fmap fixArg $ argsOf d
  where
    argsOf (TeXComm _ args) = args
    argsOf (TeXCommS _)     = []

fixArg :: TeXArg -> Latex
fixArg (FixArg a) = a
fixArg (OptArg a) = a


extract :: String -> ([TeXArg] -> a) -> Latex -> (Latex, [a])
extract name f d = ( texmap (matchName name) (const mempty) d
                   , f <$> lookForCommand name d
                   )

getCommands :: Latex -> (Latex, [Cmd])
getCommands = extract "newcommand" mkCmd

getEnvironments :: Latex -> (Latex, [Env])
getEnvironments = extract "newenvironment" mkEnv

builtInEnvs :: [Env]
builtInEnvs =
  [ Env "indent!" 1 $ \[TeXRaw (read . toS -> i)] ->
      texmap isRaw $ \(TeXRaw t) ->
        TeXRaw . T.unlines
               . fmap (mappend . toS $ replicate i ' ')
               $ T.lines t
  ]

main :: IO ()
main = do
  Config input outh <- getConfig
  parsed <- evaluate $ parseLaTeXWith parserConf input
  let !(!x, !cmds) = getCommands
                   . either (error "bad parse") id
                   $ parsed
  let !(!x', !envs) = getEnvironments x

  let !result = flip execState x'
              . replicateM 5 $ do
                  forM_ cmds                  (modify . runCmd)
                  forM_ (envs ++ builtInEnvs) (modify . runEnv)
                  modify $ either (error . show) id . parseLaTeXWith parserConf . render

  hPutStr outh . toS
               . render
               . runCmd (Cmd "}" 0 . const $ TeXRaw "}")
               . runCmd (Cmd "{" 0 . const $ TeXRaw "{")
               $ runEnv (Env "verbatim" 0 $ const id) result
  hFlush outh

