module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe, isJust)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

-- I assume that if I look at the docs for List, I'll find this exact function.
-- I didn't do that because I was instructed to reuse code from the previous
-- implementation, which it seemed to me that the book intended me to organically
-- discover `find`.
find :: forall a. (a -> Boolean) -> List a -> Maybe a
find f = head <<< filter f

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = find (\entry -> entry.firstName == firstName
                                            && entry.lastName  == lastName)

-- Ideally, I'd implement a proper equality check here, but it seems likely that
-- the book will bring that up itself sooner or later
findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress addr = find (\entry -> addr.street == entry.address.street
                                       && addr.city   == entry.address.city
                                       && addr.state  == entry.address.state)

-- The book suggests that I look up how to check if a list is empty.
-- I didn't do that, because I like code reuse. I'll look into it another time.
exists :: String -> String -> AddressBook -> Boolean
exists firstName lastName = isJust <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy (\e1 e2 -> e1.firstName == e2.firstName
                                 && e1.lastName  == e2.lastName)
