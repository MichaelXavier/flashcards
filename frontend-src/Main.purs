module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import Flashcards.Components.App as App
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, bind, map)
import Pux (renderToDOM, start)
import Pux.Router (sampleUrl)
import Signal ((~>))
import Signal.Channel (CHANNEL)
import Signal.Time (every, now, second)
-------------------------------------------------------------------------------

main
    :: forall eff. Eff ( dom :: DOM
                       , channel :: CHANNEL
                       , err :: EXCEPTION
                       , ajax :: AJAX
                       , console :: CONSOLE
                       , timer :: TIMER
                       | eff) Unit
main = do
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> App.match
  currentTime <- now
  let ticks = map App.Tick (every second)
  app <- start
    { initialState: App.initialState currentTime
    , update: App.update
    , view: App.view
      -- oddly enough, if ticks comes after routeSignal it doesn't
      -- load the page
    , inputs: [ticks, routeSignal]
    }
  renderToDOM "#app" app.html
