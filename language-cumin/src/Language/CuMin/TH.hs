{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Language.CuMin.TH
  ( cuminDecls
  , cuminExp
  , cuminPat
  , cuminBinding
  , moduleFromFile
  , cuminModule
  , dataToExp
  , module FunLogic.Core.TH
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Data
import           Data.Generics
import qualified Data.Map as Map
import Data.Default.Class
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Trifecta
import           Text.Trifecta.Indentation
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           FunLogic.Core.TH
import qualified Language.CuMin.AST as CuMin
import qualified Language.CuMin.Parser as CuMin
import qualified Language.CuMin.ModBuilder as CuMin
import qualified Language.CuMin.TypeChecker as CuMin

-- * Workaround

-- What follows is a workaround that is necessary
-- because dataToExpQ does not handle `Map`s correctly.
qqMapBnd :: Map.Map String CuMin.Binding -> Maybe (Q Exp)
qqMapBnd m = let list = dataToExpQ (const Nothing) (Map.toAscList m)
  in Just [|Map.fromAscList $list|]

qqAll :: Data a => a -> Maybe (Q Exp)
qqAll = const Nothing `extQ` qqMapADT `extQ` qqMapBnd

dataToExp :: Data a => a -> Q Exp
dataToExp = dataToExpQ qqAll

-- * QuasiQuoters

-- This cannot go into TH.hs because of cyclic module dependencies.
cuminModule :: String -> QuasiQuoter
cuminModule name = makeQQ dataToExp $ \str ->
  (CuMin.buildModuleFromDecls name <$> runParserQ CuMin.program name str)
  >>= check
  where
    check (Left msg) = fail $ "Error when building module from quasi quote:\n`" ++ show msg ++"`\n"
    check (Right m) = return m

moduleFromFile :: FilePath -> ExpQ
moduleFromFile path =
  runIO (CuMin.buildModuleFromFile path) >>= \case
    Left err -> fail $ show err
    Right m -> case CuMin.evalTC (CuMin.checkModule m) def def of
      Left err -> fail $ show $ PP.plain $ PP.pretty err
      Right _ -> dataToExp m

cuminDecls :: QuasiQuoter
cuminDecls = parserToQQ dataToExp CuMin.program

cuminExp :: QuasiQuoter
cuminExp = parserToQQ dataToExp (whiteSpace *> CuMin.expression <* whiteSpace <* eof)

cuminPat :: QuasiQuoter
cuminPat = parserToQQ dataToExp (whiteSpace *> CuMin.patternP <* whiteSpace <* eof)

cuminBinding :: QuasiQuoter
cuminBinding = parserToQQ dataToExp . absoluteIndentation $ whiteSpace *> CuMin.binding <* eof
