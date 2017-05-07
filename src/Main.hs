{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Main where

import           Control.Monad (forM_, replicateM)
import           Control.Monad.State (execState, get, modify)
import           Data.Monoid ((<>))
import           Data.String.Conv (toS)
import qualified Data.Text as T
import           Text.LaTeX.Base.Parser (parseLaTeX)
import           Text.LaTeX.Base.Render (readFileTex, render)
import           Text.LaTeX.Base.Syntax (LaTeX (..), TeXArg (..), lookForCommand, texmap)

type Latex = LaTeX

data Cmd = Cmd
  { cmdName :: String
  , cmdArgs :: Int
  , cmdCont :: [Latex] -> Latex
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

spliceLaTeX :: [Latex] -> Latex -> Latex
spliceLaTeX args d' = flip execState d'
                    . forM_ (zip [1..] args)
                    $ \(idx, arg) -> modify . texmap isRaw
                                            $ splice idx arg

isRaw :: Latex -> Bool
isRaw (TeXRaw _) = True
isRaw _          = False

splice :: Int -> Latex -> Latex -> Latex
splice idx arg (TeXRaw t) = result
  where
    Right result = parseLaTeX $ T.replace (toS $ "#" <> show idx) (render arg) t


matchName :: String -> Latex -> Bool
matchName s (TeXComm s' _) = s == s'
matchName s (TeXCommS s' ) = s == s'
matchName _ _              = False

runCmd :: Cmd -> Latex -> Latex
runCmd cmd = texmap (matchName $ cmdName cmd) $ \d -> cmdCont cmd . fixArgs $ getArgs d
  where
    getArgs (TeXComm _ args) = args
    getArgs (TeXCommS _) = []
    fixArgs as = fmap (\(FixArg a) -> a) as

getCommands :: Latex -> (Latex, [Cmd])
getCommands d = ( texmap (matchName "newcommand") (const mempty) d
                , mkCmd <$> lookForCommand "newcommand" d
                )

main :: IO ()
main = do
  Right x <- parseLaTeX <$> readFileTex "examples/test.tex"
  let (x', cmds) = getCommands x
      x'' = flip execState x' . replicateM 5 . forM_ cmds $ \cmd ->
        modify $ runCmd cmd
  putStrLn . toS $ render x''

