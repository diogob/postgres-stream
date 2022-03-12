{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import PostgresStream
import Protolude.Conv (toSL)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "GET /file/test" $ do
    it "responds with 200" $ do
      get "/file/test" `shouldRespondWith` 200
  where
    conf = Config "postgres://localhost/postgres" 8000
    app = do
      pool <- acquire (10, 10, toSL $ db conf)
      appLogger <- mkLogger
      getTime <- mkGetTime
      pure $ mkApp $ AppCtx conf appLogger pool getTime