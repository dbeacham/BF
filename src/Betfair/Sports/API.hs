module API where

import qualified Data.Text as T
import           Data.Time.Clock (UTCTime(..))
import qualified Data.HashMap.Strict as HM

newtype RunnerId = RunnerId Int
newtype CompetitionId = CompetitionId T.Text
newtype EventTypeId = EventTypeId T.Text
newtype EventId = EventId T.Text
newtype MarketId = MarketId T.Text

data MarketFilter = MarketFilter
  { mfTextQuery         :: Maybe T.Text
  , mfCompetitions      :: [CompetitionId]
  , mfEventTypes        :: [EventTypeId]
  , mfEvents            :: [EventId]
  , mfMarkets           :: [MarketId]
  , mfVenues            :: [Venue]
  , mfBspOnly           :: Maybe Bool
  , mfTurnInPlayEnabled :: Maybe Bool
  , mfInPlay            :: Maybe Bool
  , mfBettingTypes      :: [MarketBettingType]
  , mfCountries         :: [CountryCode]
  , mfMarketTypes       :: [MarketType]
  , mfStartTime         :: Maybe TimeRange
  , mfWithOrders        :: [OrderStatus]
  }

data OrderStatus
  = OrderOpen
  | OrderComplete

type SearchResults a = [SearchResult a]

data SearchResult a = SearchResult
  { srMarketCount :: Int
  , srResult      :: a
  }

data EventType = EventType
  { eventTypeId   :: EventTypeId
  , eventTypeName :: T.Text
  }

data Competition = Competition
  { competitionId     :: CompetitionId
  , competitionName   :: T.Text
  , competitionRegion :: Maybe T.Text
  -- ^ only used when search for competitions
  }

data TimeRange = TimeRange
  { from :: UTCTime
  , to   :: UTCTime
  }

data Event = Event
  { eventId          :: EventId
  , eventName        :: T.Text
  , eventCountryCode :: T.Text
  , eventTimezone    :: T.Text
  , eventVenue       :: T.Text
  , eventOpenDate    :: UTCTime
  }

newtype MarketType = MarketType T.Text
newtype CountryCode = CountryCode T.Text
newtype Venue = Venue T.Text

------------

data MarketInfo = MarketInfo
  { miId           :: MarketId
  , miName         :: T.Text                  --     VV-- Weight / MarketProjection
  , miStartTime    :: Maybe UTCTime           -- <-- 0 / MARKET_START_TIME
  , miDescription  :: Maybe MarketDescription -- <-- 1 / MARKET_DESCRIPTION
  , miTotalMatched :: Maybe Double
  , miRunners      :: Maybe [RunnerInfo]      -- <-- 0 / RUNNER_DESCRIPTION
                                              -- <-- 1 / RUNNER_METADATA
  , miEventType    :: Maybe EventType         -- <-- 0 / EVENT_TYPE
  , miCompetition  :: Maybe Competition       -- <-- 0 / COMPETITION
  , miEvent        :: Maybe Event             -- <-- 0 / EVENT
  }

data MarketDescription = MarketDescription
  { mdPersistenceEnabled :: Bool
  , mdBspMarket          :: Bool
  , mdStarTime           :: UTCTime
  , mdSuspendTime        :: UTCTime
  , mdSettleTime         :: Maybe UTCTime
  , mdBettingType        :: MarketBettingType
  , mdTurnInPlayEnabled  :: Bool
  , mdMarketType         :: MarketType
  , mdRegulator          :: T.Text
  , mdMarketBaseRate     :: Double
  , mdDiscountAllowed    :: Bool
  , mdWallet             :: Maybe T.Text
  , mdRules              :: Maybe T.Text
  , mdRulesHasDate       :: Maybe Bool
  , mdClarifications     :: Maybe T.Text
  }

data MarketBettingType
  = Odds
  | Line
  | Range
  | AsianHandicapDoubleLine
  | AsianHandicapSingleLine
  | FixedOdds

data RunnerInfo = RunnerInfo
  { riRunnerId     :: RunnerId
  , riRunnerName   :: T.Text
  , riHandicap     :: Double
  , riSortPriority :: Int
  , riMetadata     :: HM.HashMap T.Text T.Text
  }

-- ^ market information

-- ! Request
data MatchProjection
  = NoRollup
  | RollupByPrice
  | RollupByAveragePrice

data OrderByStatus
  = AllOrders
  | OrdersByStatus OrderStatus

data OrderProjection = OrderProjection
  { orderProjection :: OrderProjection
  , matchProjection :: Maybe MatchProjection
  }

data MarketSort
  = MinimumTraded
  | MaximumTraded
  | MinimumAvailable
  | MaximumAvailable
  | FirstToStart
  | LastToStart

-- ! Response
data MarketBook = MarketBook
  { marketId              :: MarketId
  , isDelayed             :: Bool
  , status                :: Maybe MarketStatus
  , betDelay              :: Maybe Int
  , bspReconciled         :: Maybe Bool
  , isComplete            :: Maybe Bool
  , isInPlay              :: Maybe Bool
  , numberOfWinners       :: Maybe Int
  , numberOfRunners       :: Maybe Int
  , numberOfActiveRunners :: Maybe Int
  , lastMatchTime         :: Maybe UTCTime
  , totalMatched          :: Maybe Double
  , totalAvailable        :: Maybe Double
  , crossMatching         :: Maybe Bool
  , runnersVoidable       :: Maybe Bool
  , version               :: Maybe Int
  , runners               :: Maybe [Runner]
  }

data MarketStatus
  = Inactive
  | Open
  | Suspended
  | Closed

data Runner = Runner
  { runnerId         :: RunnerId
  , handicap         :: Double
  , status           :: RunnerStatus
  , adjustmentFactor :: Double
  , lastPriceTraded  :: Maybe Double
  , totalMatched     :: Maybe Double
  , removalDate      :: Maybe UTCTime
  , startingPrices   :: Maybe StartingPrices -- <--  2 / SP_AVAILABLE
                                             -- <--  7 / SP_TRADED
  , exchangePrices   :: Maybe ExchangePrices -- <--  5 / EX_BEST_OFFERS
                                             -- <-- 17 / EX_ALL_OFFERS
                                             -- <-- 17 / EX_TRADED
  , orders           :: [Order]
  , matches          :: [Match]
  }

data StartingPrices = StartingPrices
  { nearPrice :: Maybe Double        -- <-- 2 / SP_AVAILABLE
  , farPrice  :: Maybe Double        -- <-- 2 / SP_AVAILABLE
  , backStakeTaken :: [PriceSize]    -- <-- 7 / SP_TRADED
  , layLiabilityTaken :: [PriceSize] -- <-- 7 / SP_TRADED
  , actualSP :: Maybe Double
  }

data ExchangePrices = ExchangePrices
  { toBack :: [PriceSize]       -- <--  5 / EX_BEST_OFFERS
                                -- <-- 17 / EX_ALL_OFFERS
  , toLay  :: [PriceSize]       -- <--  5 / EX_BEST_OFFERS
                                -- <-- 17 / EX_ALL_OFFERS
  , tradedVolume :: [PriceSize] -- <-- 17 / EX_TRADED
  }

data PriceSize = PriceSize
  { price :: Double
  , size  :: Double
  }

-- Order and match can be combined
data Order = Order
  { betId :: BetId
  , orderType :: OrderType
  , status :: OrderStatus
  , persistenceType :: PersistenceType
  , side :: Side
  , hmm :: PriceSize
  , bspLiability :: Double
  , placedAt :: UTCTime
  , averagePriceMatched :: Maybe Double
  , sizeMatched :: Maybe Double
  , sizeRemaining :: Maybe Double
  , sizeLapsed :: Maybe Double
  , sizeCancelled :: Maybe Double
  , sizeVoided :: Maybe Double
  }

data Match = Match
  { betId :: Maybe BetId
  , matchId :: Maybe BetId
  , side :: Side
  , price :: Double
  , size :: Double
  , matchedAt :: Maybe UTCTime
  }

data PriceProjection = PriceProjection
  { spAvailable    :: Bool
  , spTraded       :: Bool
  , exchangeOffers :: ExchangeOffers
  , exchangeTraded :: Bool
  }

data ExchangeOffers = ExchangeOffers
  { virtualPrices  :: Bool
  -- ^ Indicates if the returned prices should include virtual prices.
  , rollOverStakes :: Bool
  -- ^ Indicates if the volume returned at each price point should be the
  -- absolute value or a cumulative sum of volumes available at the price and
  -- all better prices.
  , offerType      :: OfferType
  }

data OfferType
  = AllOffers
  -- ^ Entire market depth
  | BestOffers { bestPricesDepth :: Int
               , rollupModel     :: Maybe RollupModel
               }
  -- ^ Only the best prices available for each runner, to requested price depth.

data RollupModel = RollupModel
  { rollupType :: RollupType
  , limit      :: Int
  }

data RollupType
  = Stake
  -- ^ The volumes will be rolled up to the minimum value which is >=
  -- rollupLimit.
  | Payout
  -- ^ The volumes will be rolled up to the minimum value where the payout
  -- (price * volume ) >= rollupLimit.

-- data MarketProjection
--   = Competition
--   | Event
--   | EventType
--   | MarketStartTime
--   | MarketDescription
--   | RunnderDescription
--   | RunnerMetaData
-- 

-- data RunnderStatus
--   = Active
--   | Winner
--   | Loser
--   | RemovedVacant
--   | Removed
--   | Hidden
-- 
-- data TimeGranularity
--   = Days
--   | Hours
--   | Minutes
-- 
-- data Side
--   = Back
--   | Lay
-- 
-- data OrderBy
--   = ByBet
--   | ByMarket
--   | ByMatchTime
--   | ByPlaceTime
--   | BySettledTime
--   | ByVoidTime
-- 
-- data SortDir
--   = EarliestToLatest
--   | LatestToEarliest
-- 
-- data OrderType
--   = Limit
--   | LimitOnClose
--   | MarketOnClose
-- 
-- data ExecutionReportStatus
--   = Success
--   | Failure
--   | ProceesedWithErrors
--   | Timeout
-- 
-- data ExecutionReportErrorCode
--   = ErrorInMatcher
--   | ProcessedWithErrors
--   | BetActionError
--   | InvalidAccountState
--   | InvalidWalletStatus
--   | InsufficientFunds
--   | LossLimitExceeded
--   | MarketSuspended
--   | MarketNotOpenForBetting
--   | DuplicateTransaction
--   | InvalidOrder
--   | InvalidMarketId
--   | PermissionDenied
--   | DuplicateBetIds
--   | NoActionRequired
--   | ServiceUnavailable
--   | RejectedByRegulator
-- 
-- data PersistenceType
--   = Lapse
--   | Persist
--   | MarketOnClose
-- 
-- data InstructionReportStatus
--   = Success
--   | Failure
--   | Timeout
-- 
-- data InstructionReportErrorCode
--   = InvalidBetSize
--   | InvalidRunner
--   | BetTakenOrLapsed
--   | BetInProgress
--   | RunnerRemoved
--   | MarketNotOpenForBetting
--   | LossLimitExceeded
--   | MarketNotOpenForBSPBetting
--   | InvalidPriceEdit
--   | InvalidOdds
--   | InsufficientFunds
--   | InvalidPersistenceType
--   | ErrorInMatcher
--   | InvalidBackLayCombination
--   | ErrorInOrder
--   | InvalidBidType
--   | InvalidBetId
--   | CancelledNotPlaced
--   | RelatedActionFailed
--   | NoActionRequired
-- 
-- data GroupBy
--   = EventType
--   | Event
--   | Market
--   | Side
--   | Bet
-- 
-- data BetStatus
--   = Settled
--   | Voided
--   | Lapsed
--   | Cancelled
