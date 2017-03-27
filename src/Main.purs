module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import DOM (DOM)
import Data.Bounded (bottom)
import Data.Date (Date)
import Data.DateTime (DateTime(..), Weekday(..), date, weekday)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (formatDateTime)
import Data.Item (Item, ItemConfig(..), addDays, comming, due, nextRun, setDone)
import Prelude (Unit, bind, map, show, unit, void, ($), (<>))
import React (ReactElement)
import React.DOM (button, div, h1, li, text, ul)
import React.DOM.Props (className, onClick)
import Thermite (EventHandler, PerformAction, Render, Spec, cotransform, defaultMain, simpleSpec)

data Action = Done Item | NextDay

type State = { heute :: Date, items :: Array Item }

initialState :: Date → State
initialState heute =
  { heute: heute
  , items: [ { text: "Papierabfuhr", executions: [], config: Weekly Tuesday }
           , { text: "Schlange", executions: [], config: Days 18 }
           , { text: "Grünabfuhr", executions: [], config: Dates [(addDays 14 heute), (addDays 28 heute), (addDays 42 heute)] }
           ]
  }

render :: Render State _ Action
render dispatch _ state _ =
  [
    div [ className "container" ]
        [
          h1 [] [ text $ showAsDate state.heute ]
        , button [ onClick (\_ -> dispatch NextDay) ] [ text "Morgen" ]
        , h1 [ className "mt-3" ]
             [ text "Heute" ]
        , ul [ className "list-group" ] $ map (renderItem Done dispatch state) (due state.heute state.items)
        , h1 [ className "mt-3" ]
             [ text "Kommend" ]
        , ul [ className "list-group" ] $ map (renderItem Done dispatch state) (comming state.heute state.items)
        ]
  ]

renderItem :: (Item → Action) → (Action → EventHandler) → State → Item → ReactElement
renderItem a dispatch state i =
  li [ onClick (\item -> dispatch $ a i), className "list-group-item" ]
     [ div [] [ text $ i.text <> show (map showAsDate i.executions)
              , text $ "Next: " <> showAsDate (nextRun state.heute i)
              ]
     ]

formatDate :: Date → String
formatDate d = always $ formatDateTime "DD.MM.YYYY" (DateTime d bottom)

always :: Either String String → String
always (Left e) = e
always (Right r) = r

translate :: Weekday → String
translate Monday = "Montag"
translate Tuesday = "Dienstag"
translate Wednesday = "Mittwoch"
translate Thursday = "Donnerstag"
translate Friday = "Freitag"
translate Saturday = "Samstag"
translate Sunday = "Sonntag"

showAsDate :: Date → String
showAsDate date = (translate $ weekday $ date) <> ", " <> formatDate date

performAction :: PerformAction _ State _ Action
performAction (Done item) _ _ =
  void $ cotransform done
    where
      done state = do
        state { items = setDone state.heute item state.items }
performAction NextDay _ _ =
  void $ cotransform $
    \state -> state { heute = addDays 1 state.heute }

spec :: Spec _ State _ Action
spec = simpleSpec performAction render

main :: ∀ e. Eff ( dom ∷ DOM , now ∷ NOW | e ) Unit
main = do
  nowInstant <- now
  let today = date $ toDateTime nowInstant
  defaultMain spec (initialState today) unit
