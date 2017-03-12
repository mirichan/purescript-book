module MiscExercises where

import Prelude (($), (+), (-))
import Data.Ord (abs)
import Data.Array (head, null)
import Data.Array.Partial (tail)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

-- The book's implementation.
length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

-- Don't disown me, Dad. The book made me do it.
isEven :: Int -> Boolean
isEven -1 = false
isEven  0 = true
isEven  i = isEven $ (abs i) - 2

-- I had to look up psc pattern matching, because
-- the suggested empty check felt gross.
-- I don't totally understand the concept of Partial
-- but I'm certain that unsafe operations are bad.
-- Hopefully I'll learn the correct way to get a tail
-- later.
evenCount :: Array Int -> Int
evenCount arr = case head arr of
    Nothing -> 0
    Just i  ->
        if isEven i then 1 + evenCount remaining
        else evenCount remaining
        where remaining = unsafePartial $ tail arr