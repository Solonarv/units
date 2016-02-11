{-# LANGUAGE
    FlexibleInstances
    #-}

module Data.Unit where

import Data.Monoid
import Data.Group
import Data.Ratio (Rational)
import Control.Applicative (liftA2)

data Dimension = Dimension
    { d_length      :: Rational -- ^ Length.                    Unit: metre    (m)
    , d_time        :: Rational -- ^ Time.                      Unit: second   (s)
    , d_mass        :: Rational -- ^ Mass.                      Unit: kilogram (kg)
    , d_current     :: Rational -- ^ Electric current.          Unit: ampere   (A)
    , d_temperature :: Rational -- ^ Thermodynamic temperature. Unit: kelvin   (K)
    , d_amount      :: Rational -- ^ Amount of substance.       Unit: mole     (mol)
    , d_lum_intesty :: Rational -- ^ Luminous intensity.        Unit: candela  (cd)
    } deriving (Eq, Show)

-- ^ The dimension of unitless quantities.
one :: Dimension
one = Dimension 0 0 0 0 0 0 0

class IsDimFieldName a where
    length', time, mass, current, temperature, amount, lum_intensity :: a

instance IsDimFieldName Dimension where
    length'       = one { d_length      = 1 }
    time          = one { d_time        = 1 }
    mass          = one { d_mass        = 1 }
    current       = one { d_current     = 1 }
    temperature   = one { d_temperature = 1 }
    amount        = one { d_amount      = 1 }
    lum_intensity = one { d_lum_intesty = 1 }



instance Monoid Dimension where
    mempty = one
    mappend d1 d2     = Dimension {
        d_length      = d_length      d1 + d_length      d2,
        d_time        = d_time        d1 + d_time        d2,
        d_mass        = d_mass        d1 + d_mass        d2,
        d_current     = d_current     d1 + d_current     d2,
        d_temperature = d_temperature d1 + d_temperature d2,
        d_amount      = d_amount      d1 + d_amount      d2,
        d_lum_intesty = d_lum_intesty d1 + d_lum_intesty d2 }

instance Group Dimension where
    invert Dimension{..} = Dimension {
        d_length      = recip d_length     
        d_time        = recip d_time       
        d_mass        = recip d_mass       
        d_current     = recip d_current    
        d_temperature = recip d_temperature
        d_amount      = recip d_amount     
        d_lum_intesty = recip d_lum_intesty }

type Unit = Dimension

data Quantity a = Quantity a Unit deriving (Eq, Show)

instance Num a => Num (Quantity a) where
    (Quantity x ux) + (Quantity y uy) = if ux == uy then Quantity (x + y) ux else error "Unit mismatch"
    (Quantity x ux) * (Quantity y uy) = Quantity (x * y) (ux <> uy)
    abs (Quantity x ux)    = Quantity x one
    signum (Quantity x ux) = Quantity 1 ux
    fromInteger i          = Quantity (fromInteger i) one
    negate (Quantity x ux) = Quantity (negate x) ux

instance Fractional a => Fractional (Quantity a) where
    recip (Quantity 

instance Num a => Num (Dimension -> Quantity a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = (abs .)
    signum = (signum .)
    fromInteger i = \dim -> Quantity (fromInteger i) dim
    negate = (negate .)