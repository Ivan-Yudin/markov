{-# LANGUAGE InstanceSigs #-} 

module Control.Markov where 

import Control.Applicative 
import Control.Monad

data Markov a b = Fail | Resume a | Done b 

data AMarkov a b = AMarkov ( a -> Markov a b ) 

markov :: r -> (a -> r) -> ( b -> r) -> 
          Markov a b -> r 

markov r _ _  Fail      = r 
markov _ f _ (Resume a) = f a 
markov _ _ h (Done b)   = h b

instance Functor (AMarkov a) where 
  fmap h (AMarkov f)   = AMarkov $ \a -> 
     markov Fail Resume  (Done . h) ( f a) 


-- To create Markov normal algorithms we need only
-- @instance Alternative (AMarkov a)@
-- Unfortunately, for the current version of Prelude, we are forced 
-- to declare @instance Applicative (AMarkov a)@. Since I didn't found an
-- easy way to define this instance, namely the @(<*>)@ method of
-- @Applicative@ class, I resorted to define @instance Monad (AMarkov a)@.
-- It is unclear what @(>>=)@ and @(<*>)@ mean in terms of normal
-- algorithms. 

instance Applicative (AMarkov a ) where
   pure b = AMarkov $ \a -> Done b 
   (<*>)  = ap

instance Monad (AMarkov a) where 
   (>>=) :: AMarkov a b -> ( b -> AMarkov a c) -> AMarkov a c 
   (AMarkov f) >>= h = AMarkov $ \a -> 
      case f a of 
       Done b    -> case h b of 
                     ~(AMarkov g ) -> g a 
       Fail      -> Fail 
       Resume a' -> Resume a' 

-- The method @(<|>)@ from the @Alternative@ class should be used to
-- construct Markov normal algorithms starting with substitution
-- primitives. 

instance Alternative  (AMarkov a) where 
   (AMarkov f ) <|> (AMarkov h) = AMarkov $ \a -> 
     markov (h a) Resume Done (f a) 

   empty = AMarkov $ \a -> Fail 

--- @sieve@ is a working horse of this package. It runs Markov normal
-- algorithm described as an element of type @AMarkov a b@ into a function
-- that consumes input of type @a@ and outputs result of type @b@ or fails. 

sieve :: AMarkov a b -> a -> Maybe b 
sieve af@(AMarkov f ) = \a -> 
    markov Nothing (sieve af) Just $ f a

-- @break@ is a primitive for creating Markov normal algorithms which do
-- not change type of input. To turn non-terminal rewriting rule into
-- terminal one, we just apply @break@ to it. 

break :: AMarkov a a -> AMarkov a a 
break (AMarkov f) = AMarkov $ \a -> 
   markov Fail Done Done (f a) 

-- @last@ is a primitive for creating Markov normal algorithms. If we use
-- @last f@ at the end of @<|>@ chain then the resulting Markov normal
-- algorithm never fails. If we are formalizing usual Markov normal
-- algorithm for which input and output types are the same, we can use
-- @last id@ at then end of $<|>@ chain. 

last :: (a -> b) -> AMarkov a b 
last  f = AMarkov $ Done . f  

