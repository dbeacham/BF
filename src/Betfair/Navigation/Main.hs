module Main where

import Navigation
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson as A

main :: IO ()
main = do
  mJson <- A.eitherDecode <$> LB.readFile "menu.json"
  case mJson :: Either String Navigation of
    Left err -> print err
    Right n  -> print n
