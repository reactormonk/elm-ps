{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module TraverseSource where

import Relude

import Language.PureScript.CST

import Data.Generics.Product
import Control.Lens

-- takes about 2 minutes to compile, so it's in a separate file.
traverseSourceToken :: Traversal' (Module ()) SourceToken
traverseSourceToken = typesUsing @Custom @SourceToken @(Module ())

data Custom
type instance Children Custom a = ChildrenCustom a

type family ChildrenCustom (a :: Relude.Type) where
  ChildrenCustom Text = '[]
  ChildrenCustom a = Children ChGeneric a
