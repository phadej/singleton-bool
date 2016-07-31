{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Singletons.Bool (
    SBool(..),
    SBoolI(..),
    -- * Data.Type.Bool
    -- | These are only defined with @base >= 4.7@
#if MIN_VERSION_base(4,7,0)
    sboolAnd, sboolOr, sboolNot,
#endif
    ) where

#if MIN_VERSION_base(4,7,0)
import           Data.Type.Bool
#endif

data SBool (b :: Bool) where
    STrue  :: SBool 'True
    SFalse :: SBool 'False

class    SBoolI (b :: Bool) where sbool :: SBool b
instance SBoolI 'True       where sbool = STrue
instance SBoolI 'False      where sbool = SFalse

-------------------------------------------------------------------------------
-- Witnesses
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,7,0)
sboolAnd :: SBool a -> SBool b -> SBool (a && b)
sboolAnd SFalse _ = SFalse
sboolAnd STrue  b = b

sboolOr :: SBool a -> SBool b -> SBool (a || b)
sboolOr STrue  _ = STrue
sboolOr SFalse b = b

sboolNot :: SBool a -> SBool (Not a)
sboolNot STrue  = SFalse
sboolNot SFalse = STrue
#endif
