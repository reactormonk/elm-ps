{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Relude

import qualified Parse.Module as ElmParse
import qualified Elm.Package
import qualified AST.Source as Elm
import qualified Reporting.Error.Syntax as ElmError
import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.CST.Print as PS
import Control.Exception
import Control.Lens
import qualified Data.Utf8 as Utf8
import qualified Data.Name as ElmName
import qualified GHC.Show as Show (Show (show))
import qualified Reporting.Annotation as ElmA
import Data.Functor.Classes
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Test.Tasty
import Test.Tasty.Golden

import TraverseSource
import Lib

main :: IO ()
main = defaultMain $ do
  goldenElm "Round"

goldenElm :: FilePath -> TestTree 
goldenElm filePath =
  -- TODO also ask the PureScript compiler
  goldenVsString filePath ("test/golden/" <> filePath <> ".purs") $ do
    elm <- parseElmFile $ "test/data/" <> filePath <> ".elm"
    pure $ encodeUtf8 $ renderPureScript $ elmToPureScript $ elm

parseElmFile :: FilePath -> IO Elm.Module
parseElmFile fp = do
  bs <- readFileBS fp
  either throwIO pure $ ElmParse.fromByteString
    (ElmParse.Package $ Elm.Package.Name ("reactormonk") ("Testing")) bs

renderPureScript :: PS.Module () -> Text
renderPureScript m =
  (PS.printTokens $ toListOf traverseSourceToken m) <> foldMap ppLc (PS.modTrailingComments m)

ppLc :: PS.Comment PS.LineFeed -> Text
ppLc = \case
  PS.Comment raw -> raw
  PS.Space n -> T.replicate n " "
  PS.Line PS.LF -> "\n"
  PS.Line PS.CRLF -> "\r\n"

-- TODO this should probably not exist, circumvents phantom type safety
instance IsString (Utf8.Utf8 a) where
  fromString = Utf8.fromChars
    
instance Show (ElmName.Name) where
  show = ElmName.toChars

instance Exception ElmError.Error
deriving instance Show ElmError.Error
deriving anyclass instance Show ElmA.Region
deriving anyclass instance Show ElmError.Module
deriving anyclass instance Show a => Show (ElmA.Located a)