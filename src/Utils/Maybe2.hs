{-# LANGUAGE DeriveFoldable #-}

module Utils.Maybe2 where

-- A data type holding either 0, 1 or 2 values of a given type.
data Maybe2 a = None | One a | Two a a deriving (Show, Foldable)

instance Eq a => Eq (Maybe2 a) where
    -- Two Maybe2 values are equal iff they have the same lengths and the same unordered elements, i.e. [1,2] = [2,1].
    None == None = True
    One x == One y = x == y
    Two a b == Two c d = a == b && c == d || a == c && b == d