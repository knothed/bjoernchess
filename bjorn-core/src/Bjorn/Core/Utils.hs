module Bjorn.Core.Utils where

import Data.Maybe

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c

lookupJust :: Eq a => a -> [(a, b)] -> b
lookupJust a = fromJust . lookup a