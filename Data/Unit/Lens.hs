{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleInstances
    #-}

module Data.Unit.Lens where

import Data.Unit.Internal
import Control.Lens

instance IsDimFieldName (ReifiedLens' Dimension Rational) where
    length'       = Lens $ lens d_length      (\dim x -> dim { d_length      = x })
    time          = Lens $ lens d_time        (\dim x -> dim { d_time        = x })
    mass          = Lens $ lens d_mass        (\dim x -> dim { d_mass        = x })
    current       = Lens $ lens d_current     (\dim x -> dim { d_current     = x })
    temperature   = Lens $ lens d_temperature (\dim x -> dim { d_temperature = x })
    amount        = Lens $ lens d_amount      (\dim x -> dim { d_amount      = x })
    lum_intensity = Lens $ lens d_lum_intesty (\dim x -> dim { d_lum_intesty = x })