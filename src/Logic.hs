module Logic where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
--import Control.Monad.Random
import System.Random

-- Lispy --FIXME no empty
data Symbol =
  Literal String
  | Not (Symbol)
  | Or (Symbol) (Symbol)
  | And (Symbol) (Symbol)
--  | T -- do I want this here?
--  | F


instance Show Symbol where --Fixme should only print necessary parens!
  show (And a b) = (show a) ++ " ∧ " ++ (show b)
  show (Or a b) = "(" ++ (show a) ++ " ∨ "  ++ (show b) ++ ")"
  show (Not s) = "¬(" ++ (show s) ++ ")"
  show (Literal s) = s


instance Eq Symbol where 
  Literal a == Literal b = a == b
  (And a b) == (And c d) = (a == c) && (b == d)
  (Or a b) == (Or c d) = (a == c) && (b == d)
  (Not s) == (Not t) = (s == t)
  _ == _ = False


--readSymbolFromString :: String -> Symbol
--readSymbolFromString

--instance Read Symbol where 


evaluate :: Symbol -> (Map.Map String Bool) -> Bool
evaluate (And s t) m = (evaluate s m) && (evaluate t m)
evaluate (Or a b) m = (evaluate a m) || (evaluate b m)
evaluate (Not s) m = (not (evaluate s m))
evaluate (Literal s) m =
  case (Map.lookup s m) of
    Nothing -> error ("Literal " ++ show s ++ " not in supplied map")
    _       -> fromJust (Map.lookup s m)


{-
http://sandbox.mc.edu/~bennet/cs110/boolalg/rules.html

The Idempotent Laws
The Associative Laws
The Commutative Laws
The Distributive Laws
The Identity Laws
The Complement Laws
The Involution Law
DeMorgan's Law
-}

idempotence  :: Symbol -> Symbol
idempotence (And s t) = if s == t then t else And (idempotence s) (idempotence t)
idempotence (Or s t) = if s == t then t else Or (idempotence s) (idempotence t)
idempotence (Not s) = Not (idempotence s) --again not sure this is the intent...
idempotence (Literal l) = Literal l

-- only (AB)C -> A(BC) supported for now
associativity  :: Symbol -> Symbol
associativity (And s c) =
  case s of 
    And a b -> And a (And b c)
    _ -> And (associativity s) (associativity c)
associativity (Or s c) =
  case s of
    Or a b -> Or a (Or b c)
    _ -> Or (associativity s) (associativity c)
associativity (Not s) = Not (associativity s) --again not sure this is the intent...
associativity (Literal l) = Literal l 

communicativity  :: Symbol -> Symbol
communicativity (And s t) = And t s 
communicativity (Or s t) = Or t s
communicativity (Not s) = Not (communicativity s) --again not sure this is the intent...
communicativity (Literal l) = Literal l 

--just left distributivity for now, right distributivity is trivial with commutativity though.
distributivity :: Symbol -> Symbol
distributivity (And s t) =
  case t of
    Or a b -> Or (And s a) (And s b) 
    _ -> And (distributivity s) (distributivity t)
distributivity (Or s t) =
  case t of 
    And a b -> And (Or s a) (Or s b)
    _ -> Or (distributivity s) (distributivity t)
distributivity (Not s) = Not (distributivity s)
distributivity (Literal l) = Literal l

-- identity law not really supportable yet; add T and F to the symbol data type as fields?

--  complement: see identity

--Todo the other way (from (not p) and (not q) to not (p and q)
de_morgan :: Symbol -> Symbol
de_morgan (And s t) = And (de_morgan s) (de_morgan t)
de_morgan (Or s t) = Or (de_morgan s) (de_morgan t)
de_morgan (Not n) = case n of
  And s t -> Or (Not s) (Not t)
  Or s t -> And (Not s) (Not t)
  _ -> Not (de_morgan n)
--de_morgan (Literal l) = error "not possible to de_morgan!"
de_morgan (Literal l) = Literal l


simplify_nots :: Symbol -> Symbol
simplify_nots (And a b) = And (simplify_nots a) (simplify_nots b)
simplify_nots (Or a b) = Or (simplify_nots a) (simplify_nots b)
simplify_nots (Not s) =
  case s of 
    Not t -> simplify_nots t --double not, removed.
    _     -> Not (simplify_nots s)
simplify_nots (Literal l) = Literal l


-- General functions
get_random_element :: [a] -> IO a
get_random_element l = do
    i <- randomRIO(0, length l - 1)
    return (l !! i)




-- Currently not used because solved better in convolute
remove_duplicats' :: (a -> a -> Bool) -> [a] -> [a]
remove_duplicats' f (x:y:xs) =
  if f x y then
    remove_duplicats' f ([x] ++ xs)
  else [x] ++ remove_duplicats' f ([y] ++ xs)
remove_duplicats' f [x] = [x]

remove_duplicats :: Eq a => [a] -> [a]
remove_duplicats [xs] = remove_duplicats' (==) [xs]
