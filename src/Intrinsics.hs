module Intrinsics(add, addl,
                  sub, subl,
                  mul, mull,
                  div, divl)
       where

import Prelude hiding (div)
import qualified Environment as E
import qualified Forms as F

add :: F.Form -> F.Form -> F.Form
add (F.Number x) (F.Number y) = F.Number (x + y)

addl :: [F.Form] -> F.Form
addl = foldl add (F.Number 0)

sub :: F.Form -> F.Form -> F.Form
sub (F.Number x) (F.Number y) = F.Number (x - y)

subl :: [F.Form] -> F.Form
subl [(F.Number x)] = F.Number (-x)
subl ((F.Number x):xs) = foldl sub (F.Number x) xs

mul :: F.Form -> F.Form -> F.Form
mul (F.Number x) (F.Number y) = F.Number (x * y)

mull :: [F.Form] -> F.Form
mull = foldl mul (F.Number 1) 

div :: F.Form -> F.Form -> F.Form
div (F.Number x) (F.Number y) = F.Number (x / y)

divl :: [F.Form] -> F.Form
divl [(F.Number x)] = F.Number (1/x)
divl ((F.Number x):xs)= foldl div (F.Number x) xs
