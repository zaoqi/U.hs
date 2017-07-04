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
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, AllowAmbiguousTypes,
UndecidableInstances, IncoherentInstances, NoMonomorphismRestriction #-}
module Data.U (
    U(),
    t,
    u
    ) where

data U :: [*] -> * where
    UOne :: x -> U (x : xs)
    USucc :: U xs -> U (x : xs)

class T1 a b where
    t1 :: (U a) -> (U b)
instance T1 a a where
    t1 = id
instance T1 xs (x : xs) where
    t1 = USucc
instance T1 (x : y : xs) (y : x : xs) where
    t1 (UOne x) = USucc (UOne x)
    t1 (USucc (UOne x)) = UOne x
    t1 (USucc (USucc xs)) = USucc (USucc xs)
instance T1 xs ys => T1 (x : xs) (x : ys) where
    t1 (UOne x) = UOne x
    t1 (USucc xs) = USucc (t1 xs)
t = t1 . t1 . t1 . t1 . t1 . t1 . t1 . t1

uone :: a -> U '[a]
uone = UOne
u x = t (uone x)

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
