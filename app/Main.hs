{-# LANGUAGE                  DataKinds #-}
{-# LANGUAGE              DeriveGeneric #-}
{-# LANGUAGE              TypeOperators #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types -- Implicitly required for `serve`
import Data.Int (Int64)
import Data.Proxy
import GHC.Generics
import Lib
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp

-- High level API description
type Api = "targets" :> Get '[JSON] [Target]
      :<|> "targets" :> "assign" :> ReqBody '[JSON] Target :> Post '[JSON] Int64
      :<|> "targets" :> "execute" :> QueryParams "targets" String :> Post '[JSON] Int64

-- TODO: Explain what it means to do this?!?!?
apiProxy :: Proxy Api
apiProxy = Proxy

data Target = Target
    { hostUrl :: String
    , targetName :: String
    , targetRequest :: Maybe String
    } deriving (Eq, Show, Generic)

instance ToJSON Target
instance FromJSON Target

dummyTarget = Target { hostUrl = "www.example.com"
                     , targetName = "Example"
                     , targetRequest = Just "Get"
                     }

server :: Server Api
server = getTargets :<|>
         assignTarget :<|>
         executeTargets where
             getTargets :: Handler [Target]
             getTargets = return $ [dummyTarget]

             assignTarget :: Target -> Handler Int64
             assignTarget _ = return $ fromInteger 200

             executeTargets :: [String] -> Handler Int64
             executeTargets [] = return $ fromInteger 200
             executeTargets _  = return $ fromInteger 501

app :: Application
app = serve apiProxy server

main :: IO ()
main = run 8000 app
