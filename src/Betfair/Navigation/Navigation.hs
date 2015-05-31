module Navigation where

data RootNode = [EventTypeNode]

data EventTypeNode = [EventTypeChildren]
data EventTypeChildren = ETGroup GroupNode
                       | ETEvent EventNode
                       | ETRace RaceNode

data RaceNode = [MarketNode]

data GroupNode = [GroupChildren]
data GroupChildren = GroupEvent EventNode
                   | GroupGroup GroupNode

data EventNode = [EventChildren]
data EventChildren = EventMarket MarketNode
                   | EventGroup GroupNode
                   | EventEvent EventNode
