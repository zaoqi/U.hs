--Copyright (C) 2017  Zaoqi

--This program is free software: you can redistribute it and/or modify
--it under the terms of the GNU Affero General Public License as published
--by the Free Software Foundation, either version 3 of the License, or
--(at your option) any later version.

--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU Affero General Public License for more details.

--You should have received a copy of the GNU Affero General Public License
--along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, GADTs,
MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, AllowAmbiguousTypes,
UndecidableInstances, IncoherentInstances, NoMonomorphismRestriction,
ScopedTypeVariables #-}

module Data.U where

data U :: [*] -> * where
    UOne :: x -> U (x : xs)
    USucc :: U xs -> U (x : xs)

class Usuccs a b where
    usuccs :: U a -> U b
instance Usuccs a a where
    usuccs = id
instance Usuccs xs ys => Usuccs (x : xs) (x : ys) where
    usuccs (UOne x) = UOne x
    usuccs (USucc xs) = USucc (usuccs xs)
instance Usuccs xs (x : xs) where
    usuccs = USucc
instance Usuccs xs ys => Usuccs xs (y : ys) where
    usuccs x = USucc (usuccs x)

u :: forall x xs. Usuccs '[x] xs => x -> U xs
u x = usuccs (UOne x :: U '[x])

class T a b where
    t :: U a -> U b
instance Usuccs '[x] t => T '[x] t where
    t (UOne x) = u x
instance (T xs t, Usuccs '[x] t) => T (x ': xs) t where
    t (UOne x) = u x
    t (USucc xs) = t xs

instance Eq x => Eq (U '[x]) where
    UOne x == UOne y = x == y
instance (Eq x, Eq (U xs)) => Eq (U (x : xs)) where
    UOne x == UOne y = x == y
    USucc xs == USucc ys = xs == ys
    _ == _ = False

instance Show x => Show (U '[x]) where
    show (UOne x) = "(u " ++ showsPrec 11 x ")"
instance (Show x, Show (U xs)) => Show (U (x : xs)) where
    show (UOne x) = "(u " ++ showsPrec 11 x ")"
    show (USucc xs) = show xs
