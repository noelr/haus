module Data.Item where

import Data.Array (filter, head)
import Data.DateTime (DateTime, Weekday(..), adjust, weekday)
import Data.DateTime (date) as DateTime
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<=), (>))
import Data.Time.Duration (Days(..)) as Duration
import Prelude (($), (-), (+), (<), (==))

data ItemConfig = Weekly Weekday | Days Int
type Item = { text :: String, executions :: Array DateTime, config :: ItemConfig }

due :: DateTime → Array Item → Array Item
due at items = filter (\i -> (nextRun at i) <= at) items

comming :: DateTime → Array Item → Array Item
comming at items = filter (\i -> (nextRun at i) > at) items

nextRun :: DateTime → Item → DateTime
nextRun default item =
  case item.config of
       Weekly wd -> if (weekday $ DateTime.date default) == wd then default else addDays (daysTill (weekday $ DateTime.date default) wd) default
       Days d ->
         case head item.executions of
           Nothing -> default
           Just date -> addDays d date

addDays :: Int → DateTime → DateTime
addDays days date = fromMaybe date $ adjust (Duration.Days (toNumber days)) date

daysTill :: Weekday → Weekday → Int
daysTill from target =
  let from' = toI from
      target' = toITatget (toI target) from'
  in target' - from'
  where
    toITatget t f = if t < f then t + 7 else t
    toI :: Weekday → Int
    toI Monday = 1
    toI Tuesday = 2
    toI Wednesday = 3
    toI Thursday = 4
    toI Friday = 5
    toI Saturday = 6
    toI Sunday = 7
