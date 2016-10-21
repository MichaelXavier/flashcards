module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import Flashcards.Components.App as App
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, bind)
import Pux (renderToDOM, start)
import Pux.Router (sampleUrl)
import Signal ((~>))
import Signal.Channel (CHANNEL)
-------------------------------------------------------------------------------

main
    :: forall eff. Eff ( dom :: DOM
                       , channel :: CHANNEL
                       , err :: EXCEPTION
                       , ajax :: AJAX
                       | eff) Unit
main = do
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> App.match
  app <- start
    { initialState: App.initialState
    , update: App.update
    , view: App.view
    , inputs: [routeSignal]
    }
  renderToDOM "#app" app.html
