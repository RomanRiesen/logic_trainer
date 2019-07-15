module Logic where
  
  --TODO FIXME the transformations should be implemented via some kind of map between the patterns, probably. So the inverse operations can be implemented easy.
  --e.g. identity_laws_map = [((And s T), s), ((And s F), F), ..]

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Either (fromRight)
--import Data.Word
--import Control.Monad.Random
import System.Random
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
--import Text.Parsec.Expr
--import Text.Parsec.Token


-- Lispy --FIXME no empty
data Symbol =
  Literal String
  | Not (Symbol)
  | Or (Symbol) (Symbol)
  | And (Symbol) (Symbol)
  | T -- do I want this here?
  | F
  deriving (Eq)

instance Show Symbol where --Fixme should only print necessary parens!
  show (And a b) = "(" ++ (show a) ++ " ∧ " ++ (show b) ++ ")"
  show (Or a b) = "(" ++ (show a) ++ " ∨ "  ++ (show b) ++ ")"
  show (Not s) = "¬(" ++ (show s) ++ ")"
  show (Literal s) = s
  show (T) = "⊤"
  show (F) = "⊥"
  --show (And a b) = "(" ++ (show a) ++ " AND " ++ (show b) ++ ")"
  --show (Or a b) = "(" ++ (show a) ++ " OR "  ++ (show b) ++ ")"
  --show (Not s) = "not(" ++ (show s) ++ ")"
  --show (Literal s) = s
  --show (T) = "T"
  --show (F) = "F"

--instance Eq Symbol where  -- auto derived
  --Literal a == Literal b = a == b
  --(And a b) == (And c d) = (a == c) && (b == d)
  --(Or a b) == (Or c d) = (a == c) && (b == d)
  --(Not s) == (Not t) = (s == t)
  --_ == _ = False

errorSymbol = Literal "ERROR"

-- parsing! Only parse the symbols 'and', 'or', 'not', 'F', 'T', '(', ')'
parseLiteral :: Parser Symbol
parseLiteral = do
  first <- letter --literals may not start with number...because I say so.
  rest <- many (letter <|> digit)
  let str = first:rest
  case str of
    "T" -> return T
    "F" -> return F
    _ -> return $ Literal str


parseNot :: Parser Symbol
parseNot = do
  char '('
  string "not"
  --try $ char '('
  space
  sym <- parseExpr
  char ')'
  return $ Not sym


-- (a and b) or c and d -> (Or (And a b) (And c d))
parseBinary :: String -> (Symbol -> Symbol -> Symbol) -> Parser Symbol
parseBinary s sym = do
  char '('
  a <- parseExpr
  space
  string s
  space
  b <- parseExpr
  char ')'
  return $ sym a b

-- at first skip everything in parentheses then parse nots before ands before ors...no clue how to do this yet. So simplest working solution first.
parseExpr :: Parser Symbol
parseExpr = do
  res <- (try $ parseBinary "and" And) 
     <|> (try $ parseBinary "or" Or)
     <|> (try parseNot)
     <|> parseLiteral 

  return res

readExpr :: String -> Symbol
readExpr input = fromRight errorSymbol (parse parseExpr "logic" input)

--instance Read Symbol where 
instance Read Symbol where
  readsPrec _ s = [(readExpr s,"")]


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

idempotenceInvAnd :: Symbol -> Symbol
idempotenceInvAnd t = And t t
idempotenceInvOr :: Symbol -> Symbol
idempotenceInvOr t = Or t t


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

commutativity  :: Symbol -> Symbol
commutativity (And s t) = And t s 
commutativity (Or s t) = Or t s
commutativity (Not s) = Not (commutativity s) --again not sure this is the intent...
commutativity l = l
--self inverse
commutativityInv = commutativity

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

-- THIS FEELS VERY WRONG AND NOT THOUGHT THROUGH!
identity_lawsInvAnd :: Symbol -> Symbol
identity_lawsInvAnd t = And t F
identity_lawsInvOr :: Symbol -> Symbol
identity_lawsInvOr t = Or t T


complement_law :: Symbol -> Symbol
complement_law (And s (Not t)) = if s == t then F else (complement_law s) `And` (Not (complement_law t))
complement_law (Or s (Not t)) = if s == t then T else (complement_law s) `Or` (Not (complement_law t))
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

get_random_element_gen :: RandomGen g => [a] -> g -> (a, g)
get_random_element_gen l gen = do
  let (i, g) = randomR (0, length l -1) gen
  (l !! i, g)



--applies n (Symbol->Symbol) functions randomly chosen from list to sym and returns the new formulas with a string that names the operation performed
convolute :: Int -> [((Symbol -> Symbol), String)] -> Symbol -> IO [(Symbol, String)]
convolute n l (Literal "ERROR") = return [(errorSymbol, "Parse Error")] --FIXME should use errorLiteral or some macro version of that
convolute n [] sym = error "Imposssible to convolute"
convolute n l sym = do
 --TODO should only search for elements in l that actually change sym. -> filter
  (f, e) <- (get_random_element l)
  let sym' = f sym
  if sym' == sym then --if this symbol already is in the list it is not new (duh.)
    convolute n l sym
  else
      if n-1 > 0 then do
        rest <- convolute (n-1) l sym'
        return ([(sym', e)] ++ rest)
      else
        return [(sym', e)]

-- same as convolute but strips the explanation strings
convolute' :: Int -> [(Symbol -> Symbol)] -> Symbol -> IO [Symbol]
convolute' n l sym = do
  c <- convolute n (zip l $ repeat "") sym
  let c' = fst $ unzip c
  return c'



-- (helper) type that holds 2 lists, one of all the functions ()
{-# LANGUAGE DuplicateRecordField #-} --FIXME actually use  this. 
--FIXME maybe use a state monad?
type Explanation = String

data PossibleStep = PossibleStep {function :: (Symbol -> Symbol), possibleStepExplanation :: Explanation}
toFunctionList :: [PossibleStep] -> [Symbol -> Symbol]
toFunctionList steps = fmap function steps

data AppliedStep = AppliedStep {symbol :: Symbol, appliedStepExplanation :: Explanation}
toSymbolList :: [AppliedStep] -> [Symbol]
toSymbolList steps = fmap symbol steps

applyStep :: PossibleStep -> Symbol -> AppliedStep
applyStep possible_step sym = do 
  let new_sym = (function possible_step) sym
  AppliedStep new_sym (possibleStepExplanation possible_step)


all_possible_steps = [ --FIXME maybe put these function definitions in a module, then MAYBE I can create this list with metaprogramming?
  PossibleStep idempotence "idempotence"
  , PossibleStep idempotenceInvAnd "idempotence"
  , PossibleStep idempotenceInvOr "idempotence"
  , PossibleStep commutativity "commutativity"
  , PossibleStep distributivity "distributivity"
  , PossibleStep identity_laws "identity"
  , PossibleStep identity_lawsInvAnd "identity"
  , PossibleStep identity_lawsInvOr "identity"
  , PossibleStep complement_law "complement"
  , PossibleStep de_morgan "de morgan" ]

  --all_expanding_steps = []

  --ll_contracting_steps = []

--data Functions = Functions {usable :: [PossibleStep], all :: [PossibleStep]}
-- improved version because the old one was a mess, but it still feels very imperativ-y
-- Applies random function in valid_functions then applies random function from all_functions n-1 times. Avoids duplicate formulas.
-- sym: Symbol to be convoluted
-- max_n: max number of formulas
-- gen: the generator used (mkStdGen seed, probably)
-- possiblble_steps: array of PossibleStep from which to randomly apply elements
convolute2 :: RandomGen g => Symbol -> Int -> g -> [PossibleStep] -> [AppliedStep] -> [AppliedStep]
convolute2 sym max_n gen possible_steps symbols_so_far = do
  -- choose function to apply
  let (possible_step, new_gen) = iter possible_steps gen where {
    iter iter_functions iter_gen =
     case iter_functions of 
      [] -> error "Not possible to convolute"
      _ -> do
        let (possible_step, new_gen) = get_random_element_gen iter_functions iter_gen
        -- apply function
        let new_sym = (function possible_step) sym
        -- check if function results in a symbol that is already in the list of lower ns
        if new_sym `elem` (toSymbolList symbols_so_far) then
          -- if it is, recurse without the tried function
          iter [x | x <- iter_functions, possibleStepExplanation x /= possibleStepExplanation possible_step] new_gen --FIXME could map all f in iter_functions over sym -> all f just executed once instead of up to |f|^2 times
        else 
          (possible_step, new_gen)
  }
  -- f sym is unique amongst the previous symbols
  let applied_step = applyStep possible_step sym 
  if max_n - 1 > 0 then
    convolute2 (symbol applied_step) (max_n - 1) new_gen possible_steps $ [applied_step] ++ symbols_so_far
  else 
    [applied_step] ++ symbols_so_far


simple_convolute_seeded :: Symbol -> Int -> Int -> [PossibleStep] -> IO [AppliedStep]
simple_convolute_seeded sym max_n seed possible_steps = do
  let gen = mkStdGen seed
  return $ convolute2 sym max_n gen possible_steps [AppliedStep sym "Start"]
  

--simple_convolute :: Symbol -> Int -> [PossibleStep] -> IO [AppliedStep]
--simple_convolute sym max_n possible_steps = do
  --s <- randomRIO (0, 2**32-1.0)
  --let gen = mkStdGen s
  --return $ convolute2 sym max_n gen possible_steps [AppliedStep sym "Start"]

data SymbolTreeNode = Binary (Symbol -> Symbol -> Symbol) | Unary (Symbol -> Symbol) | Nullary Symbol

--FIXME this could be expressed nice with use of SymbolTreeNode
data OperatorConstructors = OperatorConstructors {
  binaryOperators :: [Symbol -> Symbol -> Symbol]
, unaryOperators :: [Symbol -> Symbol]
, nullaryOperators :: [Symbol] -- terminal symbols, e.g. literals
}

operatorConstructors = OperatorConstructors [And, Or] [Not] $ [T, F] ++ fmap (\c -> Literal [c]) ['a'..'z']

-- FIXME quite ugly, improve OperatorConstructors to make this simpler
generate_random_symbol_tree_node :: RandomGen gen => OperatorConstructors -> Float -> gen -> (SymbolTreeNode, gen)
generate_random_symbol_tree_node opcs prob gen = do
  let (rprob, gen') = randomR (0.0, 1.0) gen
  if rprob < prob then do
    let t = get_random_element_gen (nullaryOperators opcs) gen'
    (Nullary $ fst t, snd t)
  else do
    let binaryOpLength = length $ binaryOperators opcs
    let unaryOpLength = length $ unaryOperators opcs
    let (num_picked, gen'') = randomR (0, binaryOpLength + unaryOpLength) gen'
    if num_picked > (binaryOpLength - 1) then do
      let t = get_random_element_gen (binaryOperators opcs) gen''
      (Binary $ fst t, snd t)
    else do
      let t = get_random_element_gen (unaryOperators opcs) gen''
      (Unary $ fst t, snd t)


--prob: probability for each new node that it will be a leaf
generate_formula :: (RandomGen gen) => Float -> gen -> (Symbol, gen)
generate_formula prob gen = do
  let get_tree_node = generate_random_symbol_tree_node operatorConstructors prob
  let (symnt, gen') = get_tree_node gen
  case symnt of
    Nullary s -> (s, gen')
    Unary s -> (s $ sym, gen'') where (sym, gen'') = generate_formula prob gen'
    Binary s -> do 
      let (sym, gen'') = generate_formula prob gen'
      let (sym', gen''') = generate_formula prob gen''
      (s sym sym', gen''')







-- Currently not used because solved better in convolute
remove_duplicats' :: (a -> a -> Bool) -> [a] -> [a]
remove_duplicats' f (x:y:xs) =
  if f x y then
    remove_duplicats' f ([x] ++ xs)
  else [x] ++ remove_duplicats' f ([y] ++ xs)
remove_duplicats' f [x] = [x]

remove_duplicats :: Eq a => [a] -> [a]
remove_duplicats [xs] = remove_duplicats' (==) [xs]

--other stuff
applyPairwise :: (a -> a -> b) -> [a] -> [b]
applyPairwise pairF l = zipWith pairF l $ tail l

--holdsPairwise :: (a -> a -> b) -> (b -> b -> b) -> [a] -> b
--holdsPairwise pairF collectorF (x:xs) = case xs of
  --(y:[xss]) -> collectorF (pairF x y) (holdsPairwise pairF collectorF xs)
  --(y:[]) -> pairF x y
