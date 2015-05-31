{-# LANGUAGE OverloadedStrings #-}

module Navigation where

import           Control.Applicative ((<|>), (<$>), (<*>), pure, empty)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Time.Clock (UTCTime)

data Navigation
  = EventTypeNode EventType
  | GroupNode Group
  | EventNode Event
  | RaceNode Race
  | MarketNode Market
  deriving (Show)

data EventType = EventType
  { _eventTypeId :: Int
  , _eventTypeName :: T.Text
  , _eventTypeChildren :: [Navigation]
  } deriving (Show)

data Group = Group
  { _groupId :: Int
  , _groupName :: T.Text
  , _groupChildren :: [Navigation]
  } deriving (Show)

data Event = Event
  { _eventId :: Int
  , _eventName :: T.Text
  , _eventCountryCode :: T.Text
  , _eventChildren :: [Navigation]
  } deriving (Show)

data Race = Race
  { _raceId :: T.Text
  , _raceName :: T.Text
  , _raceStartTime :: UTCTime
  , _raceVenue :: T.Text
  , _raceCountryCode :: T.Text
  , _raceChildren :: [Navigation]
  } deriving (Show)

data Market = Market
  { _marketExchangeId :: Int
  , _marketId :: T.Text
  , _marketStartTime :: UTCTime
  , _marketMarketType :: T.Text
  , _marketNumberOfWinners :: Maybe Int
  , _marketName :: T.Text
  } deriving (Show)

parseId :: Integral a => T.Text -> Parser a
parseId = either fail (pure . fst) . decimal
  
instance FromJSON Navigation where
  parseJSON = withObject "Navigation" $ \o -> do
    nodeType <- o .: "type"
    case nodeType :: T.Text of
      "EVENT_TYPE" -> EventTypeNode <$> parseJSON (Object o)
      "GROUP"      -> GroupNode <$> parseJSON (Object o)
      "EVENT"      -> EventNode <$> parseJSON (Object o)
      "RACE"       -> RaceNode <$> parseJSON (Object o)
      "MARKET"     -> MarketNode <$> parseJSON (Object o)
      _            -> empty
     
instance FromJSON EventType where
  parseJSON = withObject "EventType" $ \o ->
    EventType <$> (parseId =<< o .: "id")
              <*> o .: "name"
              <*> o .: "children"
     
instance FromJSON Group where
  parseJSON = withObject "Group" $ \o ->
    Group <$> ((parseId =<< o .: "id") <|> o .: "id")
          <*> o .: "name"
          <*> o .: "children"
    
instance FromJSON Event where
  parseJSON = withObject "Event" $ \o ->
    Event <$> (parseId =<< o .: "id")
          <*> o .: "name"
          <*> o .: "countryCode"
          <*> o .: "children"
    
instance FromJSON Race where
  parseJSON = withObject "Race" $ \o ->
    Race <$> o .: "id"
         <*> o .: "name"
         <*> o .: "startTime"
         <*> o .: "venue"
         <*> o .: "countryCode"
         <*> o .: "children"

instance FromJSON Market where
  parseJSON = withObject "Market" $ \o ->
    Market <$> (parseId =<< o .: "exchangeId")
           <*> o .: "id"
           <*> o .: "marketStartTime"
           <*> o .: "marketType"
           <*> (o .: "numberOfWinners" <|> pure Nothing)
           <*> o .: "name"
