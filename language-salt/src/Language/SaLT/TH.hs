{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.SaLT.TH
  ( saltDecls
  , saltExp
  , saltPat
  , saltBinding
  , saltModule
  , moduleFromFile
  , moduleFromFileWithPrelude
  , dataToExp
  , module FunLogic.Core.TH
  ) where

import           Control.Applicative
import           Data.Data
import           Data.Default.Class
import           Data.Generics
import           Data.Map
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta
import           Text.Trifecta.Indentation

import           FunLogic.Core.TH
import qualified Language.SaLT.AST            as AST
import qualified Language.SaLT.ModBuilder     as SaLT
import qualified Language.SaLT.Parser         as SaLT
import qualified Language.SaLT.TypeChecker    as SaLT

-- * Workaround

-- What follows is a workaround that is necessary
-- because dataToExpQ does not handle `Map`s correctly.
qqMapBnd :: Map String AST.Binding -> Maybe (Q Exp)
qqMapBnd m = let list = dataToExpQ (const Nothing) (toAscList m)
  in Just [|fromAscList $list|]

qqAll :: Data a => a -> Maybe (Q Exp)
qqAll = const Nothing `extQ` qqMapADT `extQ` qqMapBnd

dataToExp :: Data a => a -> Q Exp
dataToExp = dataToExpQ qqAll

-- * QuasiQuoters

saltModule :: String -> QuasiQuoter
saltModule name = makeQQ dataToExp $ \str ->
  (SaLT.buildModuleFromDecls name <$> runParserQ SaLT.program name str)
  >>= check
  where
    check (Left msg) = fail $ "Error when building module from quasi quote:\n`" ++ show (PP.plain msg) ++"`\n"
    check (Right m) = return m

moduleFromFile :: FilePath -> ExpQ
moduleFromFile = moduleFromFileWithPrelude (SaLT.emptyModule "Empty")

moduleFromFileWithPrelude :: AST.Module -> FilePath -> ExpQ
moduleFromFileWithPrelude prelude path =
  runIO (SaLT.buildModuleFromFile path) >>= \case
    Left err -> fail $ show err
    Right m -> case SaLT.evalTC (SaLT.unsafeIncludeModule prelude >> SaLT.checkModule m) def def of
      Left err -> fail $ show $ PP.plain $ PP.pretty err
      Right _ -> dataToExp m

saltDecls :: QuasiQuoter
saltDecls = parserToQQ dataToExp SaLT.program

saltExp :: QuasiQuoter
saltExp = parserToQQ dataToExp (whiteSpace *> SaLT.expression <* whiteSpace <* eof)

saltPat :: QuasiQuoter
saltPat = parserToQQ dataToExp (whiteSpace *> SaLT.patternP <* whiteSpace <* eof)

saltBinding :: QuasiQuoter
saltBinding = parserToQQ dataToExp . absoluteIndentation $ whiteSpace *> SaLT.binding <* eof
