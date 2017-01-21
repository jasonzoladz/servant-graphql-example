{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Characters where

import           BasicPrelude           hiding (empty)
import           Control.Applicative    (Alternative, empty, liftA2)
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

-- I still don't think this is cumbersome enough to bring lens

id_ :: Character -> ID
id_ (Left  x) = _id_ . _droidChar $ x
id_ (Right x) = _id_ . _humanChar $ x

name :: Character -> Text
name (Left  x) = _name . _droidChar $ x
name (Right x) = _name . _humanChar $ x

friends :: Character -> [ID]
friends (Left  x) = _friends . _droidChar $ x
friends (Right x) = _friends . _humanChar $ x

appearsIn :: Character -> [Int]
appearsIn (Left  x) = _appearsIn . _droidChar $ x
appearsIn (Right x) = _appearsIn . _humanChar $ x

secretBackstory :: Character -> Text
secretBackstory = error "secretBackstory is secret."


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

-- ** Helper functions

getHero :: Int -> Character
getHero 5 = luke
getHero _ = artoo

getHeroIO :: Int -> IO Character
getHeroIO = pure . getHero

getHuman :: Alternative f => ID -> f Character
getHuman = fmap Right . getHuman'

getHuman' :: Alternative f => ID -> f Human
getHuman' "1000" = pure luke'
getHuman' "1001" = pure vader'
getHuman' "1002" = pure han'
getHuman' "1003" = pure leia'
getHuman' "1004" = pure tarkin'
getHuman' _      = empty

getDroid :: Alternative f => ID -> f Character
getDroid = fmap Left . getDroid'

getDroid' :: Alternative f => ID -> f Droid
getDroid' "2000" = pure threepio'
getDroid' "2001" = pure artoo'
getDroid' _      = empty

getFriends :: Character -> [Character]
getFriends char = catMaybes $ liftA2 (<|>) getDroid getHuman <$> friends char

getEpisode :: Alternative f => Int -> f Text
getEpisode 4 = pure "NEWHOPE"
getEpisode 5 = pure "EMPIRE"
getEpisode 6 = pure "JEDI"
getEpisode _ = empty
