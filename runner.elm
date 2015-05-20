import Window
import Signal exposing ((<~), (~), constant)
import Graphics.Element exposing (container, middle)
import Graphics.Collage exposing (collage, rect, filled, move)
import Color exposing (darkBlue, white)

canvasWidth = 600
canvasHeight = 300
clearColor = darkBlue

type alias State = {
  x : Float,
  y : Float
}

initialState = {
  x = -10,
  y = -10
  }

display (w, h) state =
  container w h middle (
    collage canvasWidth canvasHeight [
      rect canvasWidth canvasHeight |> filled clearColor,
      rect 20 20 |> filled white |> move (state.x, state.y)
    ]
  )

main = display <~ Window.dimensions ~ constant initialState
