{-# LANGUAGE DeriveFunctor, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Tuura.Stream (
    -- * Events and streams
    Event (..), Id (..), Stream (..),

    -- * Stream manipulation
    getId, eval, getStream, setStream
    ) where

import Data.List
import Data.Foldable
import Data.Typeable
import Control.Applicative

-- | Events.
newtype Event a = Event a deriving (Eq, Functor, Ord, Show)

-- | Typed identifiers.
newtype Id a = Id Int deriving (Eq, Num, Show)

-- | Typed streams.
data Stream a where
    Source :: Typeable a               => Id a      -> [Event a]              -> Stream a
    Map    :: (Typeable a, Typeable b) => Id b      -> (a -> b)   -> Stream a -> Stream b
    Merge  :: Typeable a               => Id a      -> [Stream a]             -> Stream a
    Join   :: (Typeable a, Typeable b) => Id (a, b) -> Stream a   -> Stream b -> Stream (a, b)

-- | Extract a stream of events from a stream.
eval :: Stream a -> [Event a]
eval (Source _ s)     = s
eval (Map    _ f s)   = map (fmap f) (eval s)
eval (Merge  _ ss)    = merge (map eval ss)
eval (Join   _ sa sb) = [ Event (a, b) | (Event a, Event b) <- zip (eval sa) (eval sb) ]

-- Not sure if this is the right merge, just playing around :)
-- | Interleave a list of event lists.
merge :: [[Event a]]-> [Event a]
merge = concat . transpose

-- | Extract the identifier of a stream.
getId :: Stream a -> Id a
getId (Source id _)   = id
getId (Map    id _ _) = id
getId (Merge  id _)   = id
getId (Join   id _ _) = id

-- | Return the first substream that matches a given identifier.
getStream :: (Typeable a, Typeable b) => Id b -> Stream a -> Maybe (Stream b)
getStream id sa = case cast sa of
    Nothing -> continue
    Just sb -> if id == getId sb then Just sb else continue
  where
    continue = case sa of
        Source _ _     -> Nothing
        Map    _ _ s   -> getStream id s
        Merge  _ ss    -> asum $ map (getStream id) ss
        Join   _ s1 s2 -> getStream id s1 <|> getStream id s2

-- | Find all substreams that match a given identifier, and replace them with
-- the provided stream.
setStream :: (Typeable a, Typeable b) => Id b -> Stream b -> Stream a -> Stream a
setStream id newsb sa = case cast (sa, newsb) of
    Nothing -> continue
    Just (sb, newsa) -> if id == getId sb then newsa else continue
  where
    continue = case sa of
        Source _ _      -> sa
        Map   i f s   -> Map i f (setStream id newsb s)
        Merge i ss    -> Merge i (map (setStream id newsb) ss)
        Join  i s1 s2 -> Join i (setStream id newsb s1) (setStream id newsb s2)
