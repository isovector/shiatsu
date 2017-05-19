{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Monad (forM_, replicateM)
import           Control.Monad.State (execState, get, modify)
import           Data.Bool (bool)
import           Data.Monoid ((<>))
import           Data.String.Conv (toS)
import qualified Data.Text as T
import           System.Environment
import           System.IO (openFile, Handle, stdout, stdin, IOMode (..), hGetContents, hPutStr, hFlush)
import           Text.LaTeX.Base.Parser (parseLaTeX)
import           Text.LaTeX.Base.Render (readFileTex, render)
import           Text.LaTeX.Base.Syntax (LaTeX (..), TeXArg (..), lookForCommand, texmap)

type Latex = LaTeX

data Config = Config
  { configInput  :: Handle
  , configOutput :: Handle
  }

getConfig :: IO Config
getConfig = do
  [inf, outf] <- getArgs
  Config <$> (bool (openFile inf ReadMode)   (pure stdin)  $ inf  == "-")
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
        Right result = parseLaTeX $ T.replace (toS $ "#" <> show idx) (render arg) t

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
    envCont env (fixArgs args) d

runCmd :: Cmd -> Latex -> Latex
runCmd cmd = texmap (matchName $ cmdName cmd) $ \d ->
    cmdCont cmd . fixArgs $ argsOf d
  where
    argsOf (TeXComm _ args) = args
    argsOf (TeXCommS _)     = []

fixArgs :: [TeXArg] -> [Latex]
fixArgs = fmap (\(FixArg a) -> a)

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
  Config inh outh <- getConfig
  (x, cmds) <- getCommands . either (error "bad parse") id
                           . parseLaTeX
                           . toS
                         <$> hGetContents inh
  let (x', envs) = getEnvironments x

  let result = flip execState x'
             . replicateM 5 $ do
                 forM_ cmds                  (modify . runCmd)
                 forM_ (envs ++ builtInEnvs) (modify . runEnv)
                 modify $ fromRight . parseLaTeX . render

  hPutStr outh . toS
               . render
               . runCmd (Cmd "}" 0 . const $ TeXRaw "}")
               . runCmd (Cmd "{" 0 . const $ TeXRaw "{")
               $ runEnv (Env "verbatim" 0 $ const id) result
  hFlush outh

fromRight :: Either a b -> b
fromRight (Right b) = b

