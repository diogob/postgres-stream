module Main where

import Data.String (String)
import Env
import Network.Wai.Handler.Warp
import PostgresStream
import Protolude
import Protolude.Conv (toSL)

loadConfig :: IO Config
loadConfig =
  Env.parse (header "postgres-stream") $
    Config <$> var (str <=< nonempty) "STREAM_DB_URI" (help "Database to to store signatures")
      <*> var (auto <=< nonempty) "STREAM_PUBLIC_PORT" (help "Public port for the http server")

main :: IO ()
main = loadConfig >>= startApp

startApp :: Config -> IO ()
startApp conf = do
  putStrLn $ ("Listening on port " :: Text) <> show portNumber
  pool <- acquire (10, 10, toSL $ db conf)
  appLogger <- mkLogger
  getTime <- mkGetTime
  let ctx = AppCtx conf appLogger pool getTime
  run portNumber $ mkApp ctx
  where
    portNumber = fromIntegral $ port conf
