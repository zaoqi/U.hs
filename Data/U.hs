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
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, AllowAmbiguousTypes
 #-}
module Data.U (
    U(),
    t,
    u
    ) where

data U :: [*] -> * where
    UOne :: x -> U (x : xs)
    USucc :: U xs -> U (x : xs)

class T a b where
    t :: a -> b
instance T a a where
    t = id
instance T (U xs) (U (x : xs)) where
    t = USucc
instance T (U (x : y : xs)) (U (y : x : xs)) where
    t (UOne x) = USucc (UOne x)
    t (USucc (UOne x)) = UOne x
    t (USucc (USucc xs)) = USucc (USucc xs)
instance T (U xs) (U ys) => T (U (x : xs)) (U (x : ys)) where
    t (UOne x) = UOne x
    t (USucc xs) = USucc (t xs)

u x = t (UOne x)
