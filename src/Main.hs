module Main where
import Logic
import System.Random


--applies n (Symbol->Symbol) functions randomly chosen from list to sym and returns a list of tuples
--FIXME it would be quite a bit more elegant to catch repeated entries in the list here
convolute :: Int -> [((Symbol -> Symbol), String)] -> Symbol -> IO [(Symbol, String)]
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


convolute' :: Int -> [(Symbol -> Symbol)] -> Symbol -> IO [Symbol]
convolute' n l sym = do
  c <- convolute n (zip l $ repeat "") sym
  let c' = fst $ unzip c
  return c'


main :: IO ()
main =
  do
    let a = Literal "a"
    let b = Literal "b"
    let c = Literal "c"
    let simplificationsZip = [(idempotence, "Idempotence"), (associativity, "Associativity"), (commutativity, "Commutativity"), (distributivity, "Distributivity"), (identity_laws, "Identity"), (de_morgan, "de Morgan")]
    let s = (a `And` b) `Or` (a `And` F)
    let start = (s, "start")

    l <- convolute 3 simplificationsZip s
    --l <- convolute 10 simplificationsZip s >>= (\x -> return $ [start] ++ x) --FIXME
    --l <- do {
    --  lalala <- convolute 10 simplificationsZip s;
    --  return ([start] ++ lalala)
    --}

    --print $ remove_duplicats' (\(a, x) (b, y) -> a == b) ([start]++l)
    putStrLn $ show s ++ " [start]"
    mapM_ (
      \(symbol, explanation) -> putStrLn $
        foldr (++) "" [ " ⊨ ", show symbol, " [", explanation, "]"] 
      ) l


--    --BinaryOperator (UnaryOperator Not (Literal "a")) Or (Literal "b")
--    --Not (And (Not (Literal "a")) (Not (Literal "b")))
    --print (simplify_nots ( de_morgan (Not a `And` b)))
    --print . simplify_nots . de_morgan $ Not $ a `And` b
    --print (applyAll (take 3 (get_random_elements (mkStdGen 0x5eed) simplificationsZip)) (a `And` b))

    --(Map.fromList [("b", False)])
    --print (And a ( Or b c))
    --print (distributivity (And a ( Or b c)))
    --print (associativity (Or (Or a b) c))
    --communicativity ( simplify_nots ( de_morgan ( de_morgan (And (Literal "b") (Literal "a")))))
    --(Map.fromList [("b", False)])
