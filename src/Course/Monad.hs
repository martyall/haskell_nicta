{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Monad where

import Course.Applicative
import Course.Bind
import Course.Core
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

{-

The only exercise here is a thinking one. The understanding that the Monad
type-class is the coming together of its sub type-classes
(`Applicative` and `Bind`). There are no coding exercises here. The purpose of 
this module is simply to provide a definition for the word "monad" and that
definition is built on previous exercises.

The monad type-class provides no additional methods to `Applicative` and `Bind`.

-}

class (Applicative f, Bind f) => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

instance Monad Id where
  return = pure
  (>>=) (Id a) phi = phi a

instance Monad List where
  return = pure
  (>>=) = flip flatMap

instance Monad Optional where
  return = pure
  (>>=) fa phi = case fa of
    Empty -> Empty
    Full a -> phi a

instance Monad ((->) t) where
  return = pure
  (>>=) fa phi = \x -> ((phi $ fa x) x)


-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  return = undefined
  (>>=) = undefined
  
instance Monad [] where
  return = undefined
  (>>=) = undefined
  
instance Monad P.Maybe where
  return = undefined
  (>>=) = undefined
