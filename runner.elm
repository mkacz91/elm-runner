import Color exposing (darkBlue, white)
import Graphics.Element exposing (Element, container, middle)
import Graphics.Collage exposing (Form, collage, rect, filled, move)
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
runnerColor = white

runnerSize = 20
jumpHeight = 60
jumpDuration = 1
dimTransitionDuration = 0.1

--------------------------------------------------------------------------------
-- Utils                                                                      --
--------------------------------------------------------------------------------

lerp : Float -> Float -> Float -> Float
lerp x y alpha = (1 - alpha) * x + alpha * y

--------------------------------------------------------------------------------
-- Input                                                                      --
--------------------------------------------------------------------------------

-- Keyboard input

type Key = Jump | Slide | None

resolveKeys : Set KeyCode -> Key
resolveKeys keysDown =
  case (member 38 keysDown, member 40 keysDown) of
    (True, False) -> Jump
    (False, True) -> Slide
    _             -> None

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
-- Runner                                                                     --
--------------------------------------------------------------------------------

type RunnerState = Running | Jumping | Sliding

type alias Runner
  = {
    state        : RunnerState,
    y            : Float,
    w            : Float,
    h            : Float,
    ySnapshot    : Float,
    dimSnapshot  : (Float, Float),
    snapshotTime : Float
  }

initialRunner : Runner
initialRunner
  = {
    state        = Running,
    y            = 0,
    w            = runnerSize,
    h            = runnerSize,
    ySnapshot    = 0,
    dimSnapshot  = (runnerSize, runnerSize),
    snapshotTime = 0
  }

runnerStep : Input -> Runner -> Runner
runnerStep input runner =
  let setState state
    = { runner |
      state <- state,
      ySnapshot <- runner.y,
      dimSnapshot <- (runner.w, runner.h),
      snapshotTime <- input.time
    }
  in if
    | input.key == Jump && runner.state /= Jumping -> setState Jumping
    | input.key == Slide && runner.state == Running -> setState Sliding
    | input.key == None && runner.state == Sliding -> setState Running
    | runner.state == Jumping -> jumpStep input runner
    | runner.state == Sliding -> slideStep input runner
    | otherwise -> runStep input runner

jumpStep : Input -> Runner -> Runner
jumpStep input runner =
  let
    dt = input.time - runner.snapshotTime
    y0 = runner.ySnapshot
    d = jumpDuration
    a = 4 * jumpHeight / (d * d)
  in
    { runner | y <- y0 + a * dt * (d - dt) }
    |> dimTransition input dimTransitionDuration runnerSize runnerSize

slideStep : Input -> Runner -> Runner
slideStep input =
  dimTransition input dimTransitionDuration (2 * runnerSize) (runnerSize / 2)

runStep : Input -> Runner -> Runner
runStep input =
  dimTransition input dimTransitionDuration runnerSize runnerSize

dimTransition : Input -> Float -> Float -> Float -> Runner -> Runner
dimTransition input duration w h runner =
  let
    dt = input.time - runner.snapshotTime
    alpha = (min duration dt) / duration
    (w0, h0) = runner.dimSnapshot
  in
    { runner |
      w <- lerp w0 w alpha,
      h <- lerp h0 h alpha
    }


--------------------------------------------------------------------------------
-- Model                                                                      --
--------------------------------------------------------------------------------

type alias Model
  = {
    runner : Runner
  }

initialModel
  = {
    runner = initialRunner
  }

step : Input -> Model -> Model
step input model
  = { model |
    runner <- runnerStep input model.runner |> handleLanding input
  }

handleLanding : Input -> Runner -> Runner
handleLanding input runner =
  if runner.state == Jumping && runner.y < 0 then
    { runner |
      state <- Running,
      y <- 0,
      ySnapshot <- 0,
      dimSnapshot <- (runnerSize, runnerSize),
      snapshotTime <- input.time
    }
  else
    runner

--------------------------------------------------------------------------------
-- Display                                                                    --
--------------------------------------------------------------------------------

runnerForm : Runner -> Form
runnerForm runner =
  rect runner.w runner.h
  |> filled runnerColor
  |> move (0, runner.y)

display : (Int, Int) -> Model -> Element
display (w, h) model =
  container w h middle (
    collage canvasWidth canvasHeight [
      rect canvasWidth canvasHeight |> filled clearColor,
      runnerForm model.runner
    ]
  )

main = display <~ Window.dimensions ~ foldp step initialModel input
