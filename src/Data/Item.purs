module Data.Item where

import Data.Array (filter, head)
import Data.DateTime (DateTime, adjust)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<=), (>))
import Data.Time.Duration (Days(..))
import Prelude (($))

type Item = { text :: String, executions :: Array DateTime, days :: Int }

due :: DateTime -> Array Item -> Array Item
due at items = filter (\i -> (nextRun at i) <= at) items

comming :: DateTime -> Array Item -> Array Item
comming at items = filter (\i -> (nextRun at i) > at) items

nextRun :: DateTime -> Item -> DateTime
nextRun default item = case head item.executions of
                            Nothing -> default
                            Just date -> addDays item.days date

addDays :: Int -> DateTime -> DateTime
addDays days date = fromMaybe date $ adjust (Days (toNumber days)) date
