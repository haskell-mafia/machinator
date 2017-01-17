{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Haskell.TH.Types (
    genTypesV1
  ) where


import           Data.List.NonEmpty (NonEmpty(..))

import           Machinator.Core
import           Machinator.Core.Data.Definition

import           P

import qualified Language.Haskell.TH as TH
import qualified X.Language.Haskell.TH.Syntax as XTH


genTypesV1 :: Definition -> TH.Dec
genTypesV1 (Definition (Name n) d) =
  case d of
    Variant (nt :| nts) ->
      XTH.data_ (XTH.mkName_ n) [] (fmap (uncurry genConV1) (nt:nts))

genConV1 :: Name -> [Type] -> TH.Con
genConV1 (Name n) ts =
  XTH.normalC_' (XTH.mkName_ n) (fmap genTypeV1 ts)

genTypeV1 :: Type -> TH.Type
genTypeV1 ty =
  case ty of
    Variable (Name tn) ->
      XTH.conT (XTH.mkName_ tn)
    GroundT g ->
      case g of
        StringT ->
          XTH.conT (XTH.mkName_ "Text")
