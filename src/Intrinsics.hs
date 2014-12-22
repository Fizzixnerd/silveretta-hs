module Intrinsics(add, addl)
       where

import qualified Environment as E
import qualified Forms as F

add :: F.Form -> F.Form -> F.Form
add (F.Number x) (F.Number y) = F.Number (x + y)

addl :: [F.Form] -> F.Form
addl = foldl add (F.Number 0)
