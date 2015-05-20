import Color exposing (darkBlue, white)
import Graphics.Element exposing (Element, container, middle)
import Graphics.Collage exposing (collage, rect, filled, move)
import Keyboard exposing (KeyCode)
import Set exposing (Set, member)
import Signal exposing ((<~), (~), sampleOn, foldp)
import Time exposing (Time, fps, inSeconds)
import Window

--------------------------------------------------------------------------------
-- Configuration                                                              --
--------------------------------------------------------------------------------

canvasWidth = 600
canvasHeight = 300
clearColor = darkBlue

--------------------------------------------------------------------------------
-- Input                                                                      --
--------------------------------------------------------------------------------

-- Keyboard input

type Key = Jump | Duck | None

resolveKeys : Set KeyCode -> Key
resolveKeys keysDown =
  case (member 38 keysDown, member 40 keysDown) of
    (True, False) -> Jump
    (False, True) -> Duck
    otherwise -> None

key : Signal Key
key = Signal.map resolveKeys Keyboard.keysDown

-- Time input

time : Signal Float
time = foldp (+) 0 (inSeconds <~ fps 30)

-- Merged input

type alias Input
  = {
    key : Key,
    time : Float
  }

input : Signal Input
input = Input <~ key ~ time

--------------------------------------------------------------------------------
-- State                                                                      --
--------------------------------------------------------------------------------

type alias State
  = {
    x : Float,
    y : Float
  }

initialState : State
initialState
  = {
    x = -10,
    y = -10
  }

step : Input -> State -> State
step input state =
  if input.key == Jump
    then { state | y <-  10 }
    else { state | y <- -10 }

--------------------------------------------------------------------------------
-- Display                                                                    --
--------------------------------------------------------------------------------

display : (Int, Int) -> State -> Element
display (w, h) state =
  container w h middle (
    collage canvasWidth canvasHeight [
      rect canvasWidth canvasHeight |> filled clearColor,
      rect 20 20 |> filled white |> move (state.x, state.y)
    ]
  )

main = display <~ Window.dimensions ~ foldp step initialState input
