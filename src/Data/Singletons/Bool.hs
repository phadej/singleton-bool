{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE FlexibleContexts #-}
#endif
-- | Additions to "Data.Type.Bool".
module Data.Singletons.Bool (
    SBool(..),
    SBoolI(..),
    fromSBool,
    withSomeSBool,
    reflectBool,
    reifyBool,
    -- * Data.Type.Dec
#if MIN_VERSION_base(4,7,0)
    -- | 'discreteBool' is available with @base >= 4.7@ (GHC-7.8)
    discreteBool,
#endif
    -- * Data.Type.Bool and .Equality
    -- | These are only defined with @base >= 4.7@
#if MIN_VERSION_base(4,7,0)
    sboolAnd, sboolOr, sboolNot,
    eqToRefl, eqCast, sboolEqRefl,
    trivialRefl,
#endif
    ) where

#if MIN_VERSION_base(4,7,0)
import           Data.Type.Bool
import           Data.Type.Dec      (Dec (..))
import           Data.Type.Equality
import           Unsafe.Coerce      (unsafeCoerce)
#endif

import Data.Proxy (Proxy (..))

-- $setup
-- >>> :set -XDataKinds -XTypeOperators
-- >>> import Data.Type.Dec (decShow)

data SBool (b :: Bool) where
    STrue  :: SBool 'True
    SFalse :: SBool 'False

class    SBoolI (b :: Bool) where sbool :: SBool b
instance SBoolI 'True       where sbool = STrue
instance SBoolI 'False      where sbool = SFalse

-- | @since 0.1.5
instance Show (SBool b) where
    showsPrec _ STrue  = showString "STrue"
    showsPrec _ SFalse = showString "SFalse"

-- | @since 0.1.5
instance Eq (SBool b) where
    _ == _ = True

-- | @since 0.1.5
instance Ord (SBool b) where
    compare _ _ = EQ

-------------------------------------------------------------------------------
-- conversion to and from explicit SBool values
-------------------------------------------------------------------------------

-- | Convert an 'SBool' to the corresponding 'Bool'.
--
-- @since 0.1.4
fromSBool :: SBool b -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

-- | Convert a normal 'Bool' to an 'SBool', passing it into a continuation.
--
-- >>> withSomeSBool True fromSBool
-- True
--
-- @since 0.1.4
withSomeSBool :: Bool -> (forall b. SBool b -> r) -> r
withSomeSBool True  f = f STrue
withSomeSBool False f = f SFalse

-------------------------------------------------------------------------------
-- reify & reflect
-------------------------------------------------------------------------------

-- | Reify 'Bool' to type-level.
--
-- >>> reifyBool True reflectBool
-- True
--
reifyBool :: forall r. Bool -> (forall b. SBoolI b => Proxy b -> r) -> r
reifyBool True  f = f (Proxy :: Proxy 'True)
reifyBool False f = f (Proxy :: Proxy 'False)

-- | Reflect to term-level.
--
-- >>> reflectBool (Proxy :: Proxy 'True)
-- True
reflectBool :: forall b proxy. SBoolI b => proxy b -> Bool
reflectBool _ = fromSBool (sbool :: SBool b)

-------------------------------------------------------------------------------
-- Discrete
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,7,0)
-- | Decidable equality.
--
-- >>> decShow (discreteBool :: Dec ('True :~: 'True))
-- "Yes Refl"
--
-- @since 0.1.5
discreteBool :: forall a b. (SBoolI a, SBoolI b) => Dec (a :~: b)
discreteBool = case (sbool :: SBool a, sbool :: SBool b) of
    (STrue,  STrue)  -> Yes Refl
    (STrue,  SFalse) -> No $ \p  -> case p of {}
    (SFalse, STrue)  -> No $ \p  -> case p of {}
    (SFalse, SFalse) -> Yes Refl
#endif

-------------------------------------------------------------------------------
-- Witnesses
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,7,0)
-- | >>> sboolAnd STrue SFalse
-- SFalse
sboolAnd :: SBool a -> SBool b -> SBool (a && b)
sboolAnd SFalse _ = SFalse
sboolAnd STrue  b = b

sboolOr :: SBool a -> SBool b -> SBool (a || b)
sboolOr STrue  _ = STrue
sboolOr SFalse b = b

sboolNot :: SBool a -> SBool (Not a)
sboolNot STrue  = SFalse
sboolNot SFalse = STrue

-- | @since 0.1.1.0
eqToRefl :: (a == b) ~ 'True => a :~: b
eqToRefl = unsafeCoerce trivialRefl

-- | @since 0.1.1.0
eqCast :: (a == b) ~ 'True => a -> b
eqCast = unsafeCoerce

-- | @since 0.1.1.0
trivialRefl :: () :~: ()
trivialRefl = Refl

-- GHC 8.10+ requires that all kind variables be explicitly quantified after
-- a `forall`. Technically, GHC has had the ability to do this since GHC 8.0,
-- but GHC 8.0-8.4 require enabling TypeInType to do. To avoid having to faff
-- around with CPP to enable TypeInType on certain GHC versions, we only
-- explicitly quantify kind variables on GHC 8.6 or later, since those versions
-- do not require TypeInType, only PolyKinds.
# if __GLASGOW_HASKELL__ >= 806
#  define KVS(kvs) kvs
# else
#  define KVS(kvs)
# endif

-- | Useful combination of 'sbool' and 'eqToRefl'
--
-- @since 0.1.2.0
sboolEqRefl :: forall KVS(k) (a :: k) (b :: k). SBoolI (a == b) => Maybe (a :~: b)
sboolEqRefl = case sbool :: SBool (a == b) of
    STrue  -> Just eqToRefl
    SFalse -> Nothing
#endif
