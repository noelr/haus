module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import DOM (DOM) as DOM
import Data.Array (cons, deleteBy)
import Data.DateTime (DateTime, date, day, month, weekday, year)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (formatDateTime)
import Data.Item (Item, addDays, comming, due, nextRun)
import Prelude (Unit, bind, map, show, unit, void, ($), (<>), (==))
import React (ReactElement)
import React.DOM (button, div, h1, li, text, ul)
import React.DOM.Props (className, onClick)
import Thermite (EventHandler, PerformAction, Render, Spec, cotransform, defaultMain, simpleSpec)

data Action = Done Item | NextDay

type State = { heute :: DateTime, items :: Array Item }

initialState :: DateTime -> State
initialState heute =
  { heute: heute
  , items: [ { text: "Papierabfuhr", executions: [], days: 14 }
           , { text: "Schlange", executions: [], days: 18 }
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

renderItem :: (Item -> Action) -> (Action -> EventHandler) -> State -> Item -> ReactElement
renderItem a dispatch state i =
  li [ onClick (\item -> dispatch $ a i), className "list-group-item" ]
     [ div [] [ text $ i.text <> show (map showAsDate i.executions)
              , text $ "Next: " <> showAsDate (nextRun state.heute i)
              ]
     ]

formatDate :: DateTime -> String
formatDate datetime = case formatDateTime "DD.MM.YYYY" datetime of
                           Left r -> r
                           Right e -> e

showAsDate :: DateTime -> String
showAsDate datetime = (show $ weekday $ date datetime) <> ", " <> formatDate datetime

performAction :: PerformAction _ State _ Action
performAction (Done item) _ _ =
  void $ cotransform done
    where
      done state = do
        let others = deleteBy (\a b -> a.text == b.text) item state.items
            updated = cons (item { executions = cons state.heute item.executions }) others
        state { items = updated }
performAction NextDay _ _ =
  void $ cotransform $
    \state -> state { heute = addDays 1 state.heute }

spec :: Spec _ State _ Action
spec = simpleSpec performAction render

main :: forall e. Eff (dom :: DOM.DOM, now :: NOW | e) Unit
main = do
  nowInstant <- now
  let today = toDateTime nowInstant
  defaultMain spec (initialState today) unit
