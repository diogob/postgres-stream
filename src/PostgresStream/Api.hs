{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module PostgresStream.Api
  ( server,
    api,
    mkApp,

    -- ** re-exports
    serve,
    Proxy,
    API,
  )
where

import PostgresStream.AppM
import PostgresStream.Domain
import PostgresStream.Handlers
import PostgresStream.Prelude
import Servant

type API = "file" :> Capture "id" Text :> StreamGet NoFraming OctetStream (SourceIO ByteString)

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = file

nt :: AppCtx -> AppM a -> Handler a
nt s x = runReaderT x s

mkApp :: AppCtx -> Application
mkApp s = serve api $ hoistServer api (nt s) server
