-- | This module provides convenience functions for creating _breadcrumb_ navigation elements.

module Halogen.Themes.Bootstrap3.Breadcrumbs
  ( CrumbTrail(..)
  , breadcrumbs
  ) where

import Prelude

import Data.Tuple

import Data.Functor (($>))

import Halogen.HTML.Target

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E

import qualified Halogen.Themes.Bootstrap3 as B

-- | A `CrumbTrail` is a zipper with a current location, and crumbs behind and in front of us.
data CrumbTrail a = CrumbTrail (Array (Tuple String (Target a))) String (Array (Tuple String (Target a)))

-- | Create a breadcrumb navigation element from an array of `Crumb`s.
breadcrumbs :: forall i. CrumbTrail i -> H.HTML i
breadcrumbs (CrumbTrail behind here inFront) =
  H.ol [ A.class_ B.breadcrumb ]
  ( map fromCrumb behind ++
    [ H.li [ A.class_ B.active ] [ H.text here ] ] ++
    map fromCrumb inFront )
  where
  fromCrumb :: Tuple String (Target i) -> H.HTML i
  fromCrumb (Tuple text cr) =
    let attr = case cr of
                 LinkTarget url -> [A.href (runURL url)]
                 DataTarget i -> [E.onClick (\_ -> E.preventDefault $> i)]
    in H.li_ [ H.a attr [ H.text text ] ]
