{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Characters where

import           BasicPrelude
import           Control.Concurrent.STM (STM, TVar, newTVarIO)
import           Data.Aeson
import           Data.Map.Strict        (fromList)
import           GHC.Generics

database :: IO (TVar (Map Text Character))
database = newTVarIO database'

database' :: Map Text Character
database' = fromList
  [ ("luke", luke)
  , ("leia", leia)
  , ("vader", vader)
  , ("3po", threepio)
  , ("tarkin", tarkin)
  ]

type ID = Text

data CharCommon = CharCommon
  { _id_       :: ID
  , _name      :: Text
  , _friends   :: [ID]
  , _appearsIn :: [Int]
  } deriving (Show, Generic)

instance FromJSON CharCommon
instance ToJSON CharCommon


data Human = Human
  { _humanChar :: CharCommon
  , homePlanet :: Text
  } deriving (Show, Generic)

instance FromJSON Human
instance ToJSON Human

data Droid = Droid
  { _droidChar      :: CharCommon
  , primaryFunction :: Text
  } deriving (Show, Generic)

instance FromJSON Droid
instance ToJSON Droid

x = toJSON luke

type Character = Either Droid Human

luke :: Character
luke = Right luke'

luke' :: Human
luke' = Human
  { _humanChar = CharCommon
      { _id_        = "1000"
      , _name       = "Luke Skywalker"
      , _friends    = ["1002","1003","2000","2001"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Tatooine"
  }

vader :: Character
vader = Right vader'

vader' :: Human
vader' = Human
  { _humanChar = CharCommon
      { _id_        = "1001"
      , _name       = "Darth Vader"
      , _friends    = ["1004"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Tatooine"
  }

han :: Character
han = Right han'

han' :: Human
han' = Human
  { _humanChar = CharCommon
      { _id_        = "1002"
      , _name       = "Han Solo"
      , _friends    = ["1000","1003","2001" ]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = mempty
  }

leia :: Character
leia = Right leia'

leia' :: Human
leia' = Human
  { _humanChar = CharCommon
      { _id_        = "1003"
      , _name       = "Leia Organa"
      , _friends    = ["1000","1002","2000","2001"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Alderaan"
  }

tarkin :: Character
tarkin = Right tarkin'

tarkin' :: Human
tarkin' = Human
  { _humanChar = CharCommon
    { _id_        = "1004"
    , _name       = "Wilhuff Tarkin"
    , _friends    = ["1001"]
    , _appearsIn  = [4]
    }
  , homePlanet = mempty
  }

threepio :: Character
threepio = Left threepio'

threepio' :: Droid
threepio' = Droid
  { _droidChar = CharCommon
      { _id_ = "2000"
      , _name = "C-3PO"
      , _friends = ["1000","1002","1003","2001" ]
      , _appearsIn = [ 4, 5, 6 ]
      }
  , primaryFunction = "Protocol"
  }

artoo :: Character
artoo = Left artoo'

artoo' :: Droid
artoo' = Droid
  { _droidChar = CharCommon
      { _id_        = "2001"
      , _name       = "R2-D2"
      , _friends    = ["1000","1002","1003"]
      , _appearsIn  = [4,5,6]
      }
  , primaryFunction = "Astrometch"
  }
