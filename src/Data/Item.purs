module Data.Item where

import Data.Array (cons, deleteBy, filter, head, nubBy, sort)
import Data.Bounded (bottom)
import Data.Date (Date, Weekday, weekday)
import Data.DateTime (DateTime(..), adjust, date) as DateTime
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord ((<=), (>), (>=), (<))
import Data.Time.Duration (Days(..)) as Duration
import Prelude (($), (-), (+), (==))

data ItemConfig = Weekly Weekday | Days Int | Dates (Array Date)
type Item = { text :: String, executions :: Array Date, config :: ItemConfig }

due :: Date → Array Item → Array Item
due at items = filter (\i -> (nextRun at i) <= at) items

comming :: Date → Array Item → Array Item
comming at items = filter (\i -> (nextRun at i) > at) items

nextRun :: Date → Item → Date
nextRun today item =
  case item.config of
       Weekly wd -> if (weekday $ today) == wd then today else addDays (daysTill (weekday today) wd) today
       Dates dates -> fromMaybe today $ head $ sort $ filter (\d -> d >= today) dates
       Days d ->
         case head item.executions of
           Nothing -> today
           Just date -> addDays d date

addDays :: Int → Date → Date
addDays days dt = maybe dt DateTime.date $ DateTime.adjust (Duration.Days (toNumber days)) (DateTime.DateTime dt bottom)

daysTill :: Weekday → Weekday → Int
daysTill from target =
  let from' = fromEnum from
      target' = toITatget (fromEnum target) from'
  in target' - from'
  where
    toITatget t f = if t < f then t + 7 else t

setDone :: Date → Item → Array Item → Array Item
setDone heute item items =
  cons (item { executions = dateUniq (cons heute item.executions) }) others
    where
      others = deleteBy (\a b -> a.text == b.text) item items
      dateUniq :: Array Date → Array Date
      dateUniq a = nubBy (\d1 d2 -> d1 == d2) a
