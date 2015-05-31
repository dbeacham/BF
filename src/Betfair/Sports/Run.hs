{-# LANGUAGE OverloadedStrings #-}

module BF where

import Control.Monad (forM_)
import Data.Time.Format
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Network.HTTP.Client
import Network.HTTP.Client.Internal
import Network.HTTP.Client.TLS
import Data.Aeson
import qualified Data.Text as T
import Data.Aeson.Types (Parser)

main :: IO ()
main = do
  rc <- loadRaceCard
  case rc of
    Left err            -> print err
    Right (RaceCard ms) -> forM_ ms $ \m -> do
      print (_marketName m)
      forM_ (_marketSelections m) $ \s -> do
        putStrLn $ "  " ++ (show $ _selectionId s) ++ "  " ++ (show $ _selectionPrice s)


loadEvents :: IO (Either String RaceCard)
loadEvents = do
  mgr <- newManager tlsManagerSettings
  parseUrl "https://www.betfair.com/sport/horse-racing"

  eitherDecode <$> responseBody <$> httpLbs req mgr

  where
    xsrftoken = "x"
    updateRequest now = updateCookies now . updateQueryString now . updateHeaders
    updateHeaders req = req { requestHeaders = [("X-Requested-With", "XMLHttpRequest")] ++ requestHeaders req }
    updateCookies now req = req { cookieJar = Just $ createCookieJar [cookie now] }
