module Logic where
  
  --TODO FIXME the transformations should be implemented via some kind of map between the patterns, probably. So the inverse operations can be implemented easy.
  --e.g. identity_laws_map = [((And s T), s), ((And s F), F), ..]

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
  | T -- do I want this here?
  | F


instance Show Symbol where --Fixme should only print necessary parens!
  show (And a b) = (show a) ++ " ∧ " ++ (show b)
  show (Or a b) = "(" ++ (show a) ++ " ∨ "  ++ (show b) ++ ")"
  show (Not s) = "¬(" ++ (show s) ++ ")"
  show (Literal s) = s
  show (T) = "⊤"
  show (F) = "⊥"


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
idempotence l = l

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
associativity l = l

communicativity  :: Symbol -> Symbol
communicativity (And s t) = And t s 
communicativity (Or s t) = Or t s
communicativity (Not s) = Not (communicativity s) --again not sure this is the intent...
communicativity l = l

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
distributivity (Not s) = Not $ distributivity s
distributivity l = l

-- identity laws not really supportable yet; add T and F to the symbol data type as fields?
identity_laws :: Symbol -> Symbol
identity_laws (And s t) = case t of
  T -> s
  F -> F
  _ -> And (identity_laws s) (identity_laws t)
identity_laws (Or s t) = case t of
  T -> T
  F -> s
  _ -> Or (identity_laws s) (identity_laws t)
identity_laws (Not t) = Not $ identity_laws t
identity_laws l = l


--  complement: see identity
complement_law :: Symbol -> Symbol
complement_law (And s (Not t)) = F 
complement_law (Or s (Not t)) = T
complement_law (Not F) = T
complement_law (Not T) = F
complement_law (And s t) = And (complement_law s) (complement_law t)
complement_law (Or s t) = Or (complement_law s) (complement_law t)
complement_law (Not t) = Not (complement_law t)
complement_law l = l



--Todo the other way (from (not p) and (not q) to not (p and q)
de_morgan :: Symbol -> Symbol
de_morgan (And s t) = And (de_morgan s) (de_morgan t)
de_morgan (Or s t) = Or (de_morgan s) (de_morgan t)
de_morgan (Not n) = case n of
  And s t -> Or (Not s) (Not t)
  Or s t -> And (Not s) (Not t)
  _ -> Not (de_morgan n)
--de_morgan (Literal l) = error "not possible to de_morgan!"
de_morgan l = l


simplify_nots :: Symbol -> Symbol
simplify_nots (And a b) = And (simplify_nots a) (simplify_nots b)
simplify_nots (Or a b) = Or (simplify_nots a) (simplify_nots b)
simplify_nots (Not s) =
  case s of 
    Not t -> simplify_nots t --double not, removed.
    _     -> Not (simplify_nots s)
simplify_nots l = l


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

--random stuff
applyPairwise :: (a -> a -> b) -> [a] -> [b]
applyPairwise pairF l = zipWith pairF l $ tail l

--holdsPairwise :: (a -> a -> b) -> (b -> b -> b) -> [a] -> b
--holdsPairwise pairF collectorF (x:xs) = case xs of
  --(y:[xss]) -> collectorF (pairF x y) (holdsPairwise pairF collectorF xs)
  --(y:[]) -> pairF x y