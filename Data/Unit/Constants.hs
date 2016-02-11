module Data.Unit.Constants (
    module Data.Unit.Constants
    ) where

import Data.Unit.Internal

-- Physical constants and some extra units, from https://en.wikipedia.org/wiki/Physical_constant

newton, joule, coulomb, volt, ohm :: Unit
newton = kg .* m ./ s .^ 2
joule = newton .* m
coulomb = amp .* s
volt = joule ./ coulomb
ohm = volt ./ amp

-- Auxiliary units
eV :: Floating a => Quantity a
eV = e * 1 volt

-- Universal constants
c, _G, h, h' :: Floating a => Quantity a
c  = 299792458       $ m ./ s                 -- speed of light in vacuum
_G = 6.67408e-11     $ m .^ 3 ./ kg ./ s .^ 2 -- Newtonian constant of gravitation
h  = 6.626070040e-34 $ joule .* s             -- Planck constant
h' = 1.054571800e-34 $ joule .* s             -- reduced Planck constant

-- Electromagnetic constants
mu0, epsilon0, _Z0, ke, e, muB, _G0, _KJ, phi0, muN, _RK :: Floating a => Quantity a
mu0      = 4 * pi (newton ./ amp .^ 2) -- magnetic constant (vacuum permeability)
epsilon0 = 1 / (mu0 * c * c)           -- electric constant (vacuum permittivity)
_Z0      =   mu0 * c      -- characteristic impedance of vacuum
ke       = 1 / (4 * pi * epsilon0)     -- Coulomb's constant
e        = 1.602176565e-19 coulomb     -- elementary charge
muB      = e * h' / (2 * me)           -- Bohr magneton
_G0      = 2 * e * e / h               -- conductance quantum
_KJ      = 2 * 2 / h                   -- Josephson constant
phi0     = h / (2 * e)                 -- magnetic flux quantum
muN      = e * h' / (2 * mp)           -- nuclear magneton
_RK      = h / (e * e)                 -- von Klitzing constant

-- Atomic and nuclear constants
a0, re, me, _GF, alpha, _Eh, mp, _Rinf, sin2_weakmix, efimov :: Floating a => Quantity a
a0           = alpha / (4 * pi * _Rinf)                 -- Bohr radius
re           = e * e * ke / (c * c)                     -- classical electronn radius
me           = 9.10938291e-31 kg                        -- electron mass
_GF          = (h' * c) ^^ 3 * 1.166364e-23 / (eV * eV) -- Fermi coupling constant (well, mostly. If you write _GF / (h' * c) ^^ 3 you get that constant.
alpha        = mu0 * e * e * c / (2 * h)                -- fine-structure constant
_Eh          = 2 * _Rinf * h * c                        -- Hartree energy
mp           = 1.672621777e-27 kg                       -- proton mass
_Rinf        = alpha * alpha * me * c / (2 * h)         -- Rydberg constant
sin2_weakmix = 0.2223                                   -- the sine-squared of the weak mixing angle
efimov       = 22.7                                     -- Efimov factor

-- Physico-chemical constants