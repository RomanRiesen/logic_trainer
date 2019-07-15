{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Server where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Network.Wai
import Network.Wai.Handler.Warp
import Servant 
import Logic

data LogicResponse = LogicResponse
  { --response_type :: String, --maybe should be enum, but enums look repulsive in this language
    formulas :: [(Symbol, String)]
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''LogicResponse)

$(deriveJSON defaultOptions{sumEncoding=ObjectWithSingleField, constructorTagModifier = map toLower} ''Symbol) -- Holy shit, aeson rocks!

type API = "api" 
    :> Capture "request_type" String
    :> QueryParam "formula" String
    :> QueryParam "steps" Int
    :> QueryParam "seed" Int --FIXME TODO !!! would be great
    :> Get '[JSON] LogicResponse
    :<|> Raw

simplificationsZip = [(idempotence, "Idempotence"), (associativity, "Associativity"), (commutativity, "Commutativity"), (distributivity, "Distributivity"), (identity_laws, "Identity"), (de_morgan, "de Morgan")]
possible_steps = [PossibleStep idempotence "idempotence", PossibleStep associativity "associativity"]
--do_the_convolution n s = liftIO $ convolute n simplificationsZip s

logic_responder :: String -> Maybe String -> Maybe Int -> Maybe Int -> Handler LogicResponse

logic_responder "train" Nothing _ _ = --FIXME implement, also probably want function to create symbol with m total child symbols
    return $ LogicResponse [((Literal "NotYetImplemented"), "sorry")]

logic_responder "convolute" (Just sym) (Just n) (Just seed) = do
    --l <- liftIO $ convolute n simplificationsZip (readExpr sym)
    l <- liftIO $ simple_convolute_seeded (readExpr sym) n seed all_possible_steps
    let pair_list = fmap (\x -> (symbol x, appliedStepExplanation x)) (reverse l)
    liftIO $ print pair_list
    return $ LogicResponse $ pair_list

logic_responder "simplify" (Just sym) (Just n) (Just seed) = do
    l <- liftIO $ convolute n simplificationsZip (readExpr sym)
    return $ LogicResponse $ [((Literal "NotYetImplemented"), "sorry!")]

logic_server :: Server API
logic_server = logic_responder :<|> serveDirectoryFileServer "./frontend"

api :: Proxy API
api = Proxy

app :: Application
app = serve api logic_server

server_main :: IO ()
server_main = do
  print "Starting Logic server on port 4510."
  run 4510 app 