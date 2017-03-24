module Data.Item where

import Data.Array (filter, head)
import Data.DateTime (DateTime, Weekday, adjust, weekday)
import Data.DateTime (date) as DateTime
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<=), (>), (<))
import Data.Time.Duration (Days(..)) as Duration
import Prelude (($), (-), (+), (==))

data ItemConfig = Weekly Weekday | Days Int
type Item = { text :: String, executions :: Array DateTime, config :: ItemConfig }

due :: DateTime → Array Item → Array Item
due at items = filter (\i -> (nextRun at i) <= at) items

comming :: DateTime → Array Item → Array Item
comming at items = filter (\i -> (nextRun at i) > at) items

nextRun :: DateTime → Item → DateTime
nextRun today item =
  case item.config of
       Weekly wd -> if (weekday $ DateTime.date today) == wd then today else addDays (daysTill (weekday $ DateTime.date today) wd) today
       Days d ->
         case head item.executions of
           Nothing -> today
           Just date -> addDays d date

addDays :: Int → DateTime → DateTime
addDays days date = fromMaybe date $ adjust (Duration.Days (toNumber days)) date

daysTill :: Weekday → Weekday → Int
daysTill from target =
  let from' = fromEnum from
      target' = toITatget (fromEnum target) from'
  in target' - from'
  where
    toITatget t f = if t < f then t + 7 else t
