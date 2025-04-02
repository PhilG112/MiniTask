{-# LANGUAGE TypeOperators #-}

module Main where

import Api.Apis
    ( AccountApi,
      LoginRequest,
      MeResponse (..),
      RegisterRequest,
      UserApi,
    )
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Network.Wai
    ( Application,
      Middleware,
      Request (rawPathInfo, requestMethod),
    )
import Network.Wai.Handler.Warp (run)
import Servant
    ( Handler,
      NoContent (..),
      Proxy (..),
      Server,
      serve,
      type (:<|>) (..),
    )

loginHandler :: LoginRequest -> Handler NoContent
loginHandler req = do
    liftIO $ putStrLn ("Login attempt")
    return NoContent

registerHandler :: RegisterRequest -> Handler NoContent
registerHandler registerReq = do
    liftIO $ putStrLn $ "Registering"
    return NoContent

logoutHandler :: Maybe T.Text -> Handler NoContent
logoutHandler mReturnUrl = do
    liftIO $ putStrLn $ "Logging out. Redirect to: " ++ show mReturnUrl
    return NoContent

meHandler :: Handler MeResponse
meHandler = return $ MeResponse {email = "test@example.com"}

type AppApi = AccountApi :<|> UserApi

accountHandlers :: Server AccountApi
accountHandlers = loginHandler :<|> registerHandler :<|> logoutHandler

userHandlers :: Server UserApi
userHandlers = meHandler

server :: Server AppApi
server = accountHandlers :<|> userHandlers

api :: Proxy AppApi
api = Proxy

app :: Application
app = serve api server

requestLogger :: Middleware
requestLogger app req respond = do
    BS.putStrLn $ "📥 " <> requestMethod req <> " " <> rawPathInfo req
    app req respond

requestLogger2 :: Middleware
requestLogger2 app req respond = do
    BS.putStrLn $ "📥 " <> requestMethod req <> " " <> rawPathInfo req
    app req respond

main :: IO ()
main = do
    putStrLn "Running on http://localhost:9090"
    let midw = requestLogger . requestLogger2
    run 9090 $ midw app