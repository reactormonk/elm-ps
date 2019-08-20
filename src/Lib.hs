module Lib where

import Relude

import qualified AST.Source as Elm
import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as PSN
import qualified Elm.ModuleName
import qualified Reporting.Annotation as ElmA
import qualified Data.Name as Name
import qualified Data.Text as T


elmToPureScript :: Elm.Module -> PS.Module ()
elmToPureScript Elm.Module {..} =
  PS.Module 
    { modAnn = ()
    , modKeyword = PS.SourceToken spaceAnn $ PS.TokLowerName [] "module"
    , modNamespace = transformName _name
    , modExports = transformExports _exports
    , modWhere = PS.SourceToken spaceAnn $ PS.TokLowerName [] "where"
    , modImports = [] -- transformImport <$> _imports -- TODO
    , modDecls = transformBody _values _unions _aliases _binops
    , modTrailingComments = []
    -- TODO effects?
    }

transformName :: Maybe (ElmA.Located Name.Name) -> PS.Name PSN.ModuleName
transformName elmN =
  let psName name splitName = PS.Name (PS.SourceToken spaceAnn (PS.TokUpperName [] name)) (PSN.ModuleName $ PSN.ProperName <$> splitName)
  in
    case elmN of
      Just name ->
        let
          n = ElmA.toValue name
        in
          psName (nameToText n) (nameToText <$> Name.splitDots n)
      Nothing -> psName "Main" ["Main"]

nameToText :: Name.Name -> Text
nameToText = T.pack . Name.toChars

transformExports :: ElmA.Located Elm.Exposing -> Maybe (PS.DelimitedNonEmpty (PS.Export ()))
transformExports loc =
  case ElmA.toValue loc of
    Elm.Open -> Nothing
    Elm.Explicit exposed -> do
      ne <- nonEmpty exposed
      let transform export =
            case export of
              Elm.Lower nameL ->
                let name = nameToText $ ElmA.toValue nameL
                in PS.ExportValue () $ PS.Name (PS.SourceToken spaceAnn $ PS.TokLowerName [] name) $ PS.Ident $ name
              Elm.Upper nameL privacy ->
                let
                  name = nameToText $ ElmA.toValue nameL
                  mainType = PS.Name (PS.SourceToken spaceAnn $ PS.TokUpperName [] $ name) $ PSN.ProperName name
                in
                  case privacy of
                    Elm.Private -> PS.ExportType () mainType Nothing
                    Elm.Public region -> PS.ExportType () mainType $ Just $ PS.DataAll () $ PS.SourceToken spaceAnn $ PS.TokSymbolName [] ".."
              Elm.Operator region name -> PS.ExportOp () $ PS.Name (PS.SourceToken spaceAnn (PS.TokSymbolName [] (nameToText name))) $ PSN.OpName $ nameToText name
      pure $ PS.Wrapped
        (PS.SourceToken spaceAnn PS.TokLeftParen) 
        (PS.Separated (transform $ head ne) ((\x -> (PS.SourceToken spaceAnn PS.TokComma, transform x)) <$> tail ne))
        (PS.SourceToken spaceAnn PS.TokRightParen) 

transformImport :: Elm.Import -> PS.ImportDecl ()
transformImport imports = undefined

transformBody :: [ElmA.Located Elm.Value] -> [ElmA.Located Elm.Union] -> [ElmA.Located Elm.Alias] -> [ElmA.Located Elm.Infix] -> [PS.Declaration ()]
transformBody values unions aliases infixes = [] -- TODO
  -- TODO sort them again
  -- fmap (transformValue . ElmA.toValue) values <>
  -- fmap (transformUnion . ElmA.toValue) unions <>
  -- fmap (transformAlias . ElmA.toValue) aliases <>
  -- fmap (transformInfix . ElmA.toValue) infixes

transformValue :: Elm.Value -> PS.Declaration ()
transformValue = undefined

transformUnion :: Elm.Union -> PS.Declaration ()
transformUnion = undefined

transformAlias :: Elm.Alias -> PS.Declaration ()
transformAlias = undefined

transformInfix :: Elm.Infix -> PS.Declaration ()
transformInfix = undefined

newlineAnn :: Int -> PS.TokenAnn
newlineAnn indent = PS.TokenAnn noSourceRange [PS.Line PS.LF, PS.Space indent] []

spaceAnn :: PS.TokenAnn
spaceAnn = PS.TokenAnn noSourceRange [PS.Space 1] []

emptyAnn :: PS.TokenAnn
emptyAnn = PS.TokenAnn noSourceRange [] []

noSourceRange :: PS.SourceRange
noSourceRange = PS.SourceRange { srcStart = PS.SourcePos 0 0, srcEnd = PS.SourcePos 0 0 }