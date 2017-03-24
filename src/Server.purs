module Server where

import Prelude (bind, show, ($), (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, message)
import Node.Express.App (App, get, listenHttp, useOnError)
import Node.Express.Handler (Handler)
import Node.Express.Response (sendFile, sendJson, setStatus)
import Node.Express.Types (EXPRESS)
import Node.HTTP (Server)

errorHandler :: ∀ e. Error → Handler e
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

indexHandler :: ∀ e. Handler e
indexHandler = do
  sendFile "index.html"

mainJsHandler :: ∀ e. Handler e
mainJsHandler = do
  sendFile "main.js"

appSetup :: ∀ e. App ( console ∷ CONSOLE | e )
appSetup = do
  liftEff $ log "Setting up"
  get "/"        indexHandler
  get "/main.js" mainJsHandler
  useOnError     errorHandler

main :: ∀ a. Eff ( express ∷ EXPRESS , console ∷ CONSOLE | a ) Server
main = do
  let port = 5000
  listenHttp appSetup port \_ ->
    log $ "Listening on " <> show port
