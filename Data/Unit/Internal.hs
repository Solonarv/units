{-# LANGUAGE
    FlexibleInstances,
    RecordWildCards,
    ViewPatterns
    #-}

module Data.Unit.Internal (
    Dimension, Unit, Quantity(..),
    one, IsDimFieldName(..),
    m, s, kg, amp, kelvin, mol, cd,
    (.*), (.^), (./),
    forceAs
    ) where

import Data.Monoid
import Data.Group
import Data.Maybe (catMaybes)
import Data.Ratio
import Control.Applicative (liftA2)

data Dimension = Dimension
    { d_length      :: Rational -- ^ Length.                    Unit: metre    (m)
    , d_time        :: Rational -- ^ Time.                      Unit: second   (s)
    , d_mass        :: Rational -- ^ Mass.                      Unit: kilogram (kg)
    , d_current     :: Rational -- ^ Electric current.          Unit: ampere   (A)
    , d_temperature :: Rational -- ^ Thermodynamic temperature. Unit: kelvin   (K)
    , d_amount      :: Rational -- ^ Amount of substance.       Unit: mole     (mol)
    , d_lum_intesty :: Rational -- ^ Luminous intensity.        Unit: candela  (cd)
    } deriving Eq

instance Show Dimension where
    show Dimension{..} = unwords $ catMaybes [lent, time, mass, curr, temp, amnt, lumi]
      where
        lent = showUnit "m"    d_length
        time = showUnit "s"    d_time
        mass = showUnit "kg"   d_mass
        curr = showUnit "A"    d_current
        temp = showUnit "K"    d_temperature
        amnt = showUnit "mol"  d_amount
        lumi = showUnit "cd"   d_lum_intesty
        showUnit uname exp = case exp `compare` 0 of
            LT -> Just ("/ " ++ uname ++ showExp (negate exp))
            EQ -> Nothing
            GT -> Just (uname ++ showExp exp)
        showExp exp = if exp == 1
            then ""
            else if denominator exp == 1
                 then "^" ++ show (numerator exp)
                 else "^" ++ show (numerator exp) ++ "/" ++ show (denominator exp)
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

m, s, kg, amp, kelvin, mol, cd :: Unit
m = length'
s = time
kg = mass
amp = current
kelvin = temperature
mol = amount
cd = lum_intensity

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
        d_length      = negate d_length      ,
        d_time        = negate d_time        ,
        d_mass        = negate d_mass        ,
        d_current     = negate d_current     ,
        d_temperature = negate d_temperature ,
        d_amount      = negate d_amount      ,
        d_lum_intesty = negate d_lum_intesty }

infixl 6 .*, ./
infixl 7 .^
(.^) :: Integral a => Dimension -> a -> Dimension
(.^) = pow
(.*) :: Dimension -> Dimension -> Dimension
(.*) = (<>)
(./) :: Dimension -> Dimension -> Dimension
x ./ y = x <> invert y

type Unit = Dimension

class HasUnit a where getUnit :: a -> Unit
instance HasUnit Unit where getUnit = id
instance HasUnit (Quantity a) where getUnit = unit

-- | Partial function to ensure that a quantity has a certain unit
forceAs :: (HasUnit u, HasUnit q) => u -> q -> q
forceAs (getUnit -> u) q = if getUnit q == u then q else error "Unit mismatch"

data Quantity a = Quantity { val :: a, unit :: Unit } deriving Eq

instance Show a => Show (Quantity a) where
    show Quantity{..} = let su = show unit in if su == "" then show val else show val ++ " " ++ su

instance Num a => Num (Quantity a) where
    (Quantity x ux) + (Quantity y uy) = if ux == uy then Quantity (x + y) ux else error "Unit mismatch"
    (Quantity x ux) * (Quantity y uy) = Quantity (x * y) (ux <> uy)
    abs (Quantity x ux)    = Quantity (abs x) one
    signum (Quantity x ux) = Quantity (signum x) ux
    fromInteger i          = Quantity (fromInteger i) one
    negate (Quantity x ux) = Quantity (negate x) ux

instance Fractional a => Fractional (Quantity a) where
    recip (Quantity x ux) = Quantity (recip x) (invert ux)
    fromRational r = Quantity (fromRational r) one

instance Floating a => Floating (Quantity a) where
    pi = Quantity pi one
    exp (Quantity x ux) = if ux == one then Quantity (exp x) ux else error "Can only exponentiate dimensionless quantities"
    log (Quantity x ux) = if ux == one then Quantity (log x) ux else error "Can only tke the logarithm of dimensionless quantities"
    sin (Quantity x ux) = if ux == one then Quantity (sin x) ux else error "Can only take the sine of dimensionless quantities"
    cos (Quantity x ux) = if ux == one then Quantity (cos x) ux else error "Can only take the cosine of dimensionless quantities"
    asin (Quantity x ux) = if ux == one then Quantity (asin x) ux else error "Can only take the arcsine of dimensionless quantities"
    acos (Quantity x ux) = if ux == one then Quantity (acos x) ux else error "Can only take the arccosine of dimensionless quantities"
    atan (Quantity x ux) = if ux == one then Quantity (atan x) ux else error "Can only take the arctangent of dimensionless quantities"
    sinh (Quantity x ux) = if ux == one then Quantity (sinh x) ux else error "Can only take the hyperbolic sine of dimensionless quantities"
    cosh (Quantity x ux) = if ux == one then Quantity (cosh x) ux else error "Can only take the hyperbolic cosine of dimensionless quantities"
    asinh (Quantity x ux) = if ux == one then Quantity (asinh x) ux else error "Can only take the hyperbolic arcsine of dimensionless quantities"
    acosh (Quantity x ux) = if ux == one then Quantity (acosh x) ux else error "Can only take the hyperbolic arccosine of dimensionless quantities"
    atanh (Quantity x ux) = if ux == one then Quantity (atanh x) ux else error "Can only take the hyperbolic arctangent of dimensionless quantities"

instance Num a => Num (Unit -> Quantity a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = (abs .)
    signum = (signum .)
    fromInteger = Quantity . fromInteger
    negate = (negate .)

instance Fractional a => Fractional (Unit -> Quantity a) where
    recip = (recip .)
    fromRational = Quantity . fromRational

instance Floating a => Floating (Unit -> Quantity a) where
    pi = Quantity pi
    exp   = (exp   .)
    log   = (log   .)
    sin   = (sin   .)
    cos   = (cos   .)
    asin  = (asin  .)
    acos  = (acos  .)
    atan  = (atan  .)
    sinh  = (sinh  .)
    cosh  = (cosh  .)
    asinh = (asinh .)
    acosh = (acosh .)
    atanh = (atanh .)