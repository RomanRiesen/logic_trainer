module Main where

import System.Random
import System.Environment

import Logic
import Server


main :: IO ()
main = server_main
-- = do
  --args <- getArgs
  --case head args of
    --"serve" -> server_main
    --_ -> normal_main


normal_main =
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
