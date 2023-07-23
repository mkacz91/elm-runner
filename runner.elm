import Browser
import Browser.Events
import Json.Decode as Decode
import List exposing (any)
import Set exposing (Set)
import Random exposing (Generator, Seed, generate, initialSeed)
import Time
import Time exposing (posixToMillis)
import Svg
import Svg exposing (Svg, svg)
import Svg.Attributes
import Html
import Html exposing (Html)
import Html.Attributes

--------------------------------------------------------------------------------
-- Configuration
--
-- This part contains configuration constants that determine the appearance,
-- behaviour, and, in consequence, the difficulty.
--------------------------------------------------------------------------------

canvasWidth = 600
canvasHeight = 120
bg = "rgb(32, 74, 135)"
fg = "white"

runnerSize = 20.0
runnerX = 200.0
jumpHeight = 50.0
jumpDuration = 0.6
dimTransitionDuration = 0.1
speed = 200.0
obstacleUnit = 30.0
minObstacleDistance = 70.0
obstacleMargin = 5
pauseMargin = 1.0
maxSpikeUnits = 3
maxHoleUnits = 5

-- Computed constants

canvasLeft = -runnerX
canvasRight = canvasWidth - runnerX
sqrt3 = sqrt(3)
spikeH = sqrt3 / 2 * obstacleUnit

--------------------------------------------------------------------------------
-- Utils
--
-- Utility functions that are not present in the core library.
--------------------------------------------------------------------------------

{-| Linear interpolation between values. -}
lerp : Float -> Float -> Float -> Float
lerp x y alpha = (1 - alpha) * x + alpha * y

{-| Calling `iter n f` produces a list of results of f applied to integers
from 0 to (n - 1). -}
iter : Int -> (Int -> a) -> List a
iter n f =
  let aux m xs = if m < 1 then xs else aux (m - 1) (f (m - 1) :: xs)
  in aux n []

{-| Drops all initial elements of a list that fulfill a predicate. -}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile f xs = case xs of
  x :: xs_ -> if f x then dropWhile f xs_ else xs
  [] -> []

{-| Retrieve the last element of a list. -}
last : List a -> Maybe a
last xs = case xs of
  [] -> Nothing
  [x] -> Just x
  _ :: xs_ -> last xs_

{-| Utility function for producing a commandless tuple. -}
cmdless : a -> (a, Cmd msg)
cmdless x = (x, Cmd.none)

--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

type Msg
  = Tick Time.Posix
  | Key Bool String
  | Touch String
  | SpawnObstacle Obstacle

applyTick : Time.Posix -> Model -> (Model, Cmd Msg)
applyTick posix model =
  let
    oldInput = model.input
    millis = posixToMillis posix
    input =
      { oldInput
      | time = toFloat (millis - model.startMillis) / 1000
      }
  in
    { model
    | millis = millis
    , input = input
    }
    |> step

keyDecoder : Bool -> Decode.Decoder Msg
keyDecoder state = Decode.map (Key state) (Decode.field "code" Decode.string)

applyKey : Bool -> String -> Model -> (Model, Cmd Msg)
applyKey state key model = cmdless <|
  let
    oldInput = model.input
    keys =
      if state
      then Set.insert key oldInput.keys
      else Set.remove key oldInput.keys
    input =
      { oldInput
      | keys = keys
      , jump = Set.member "Up" keys
          || Set.member "ArrowUp" keys
          || Set.member "RightTouch" keys
      , slide = Set.member "Down" keys
          || Set.member "ArrowDown" keys
          || Set.member "LeftTouch" keys
      }
  in
    { model | input = input }

applyTouch : String -> Model -> (Model, Cmd Msg)
applyTouch touch model0 =
  let
    (model1, cmd1) = applyKey (String.contains "L" touch) "LeftTouch" model0
    (model2, cmd2) = applyKey (String.contains "R" touch) "RightTouch" model1
  in
    (model2, Cmd.batch [cmd1, cmd2])

applySpawnObstacle : Obstacle -> Model -> (Model, Cmd Msg)
applySpawnObstacle obstacle model = cmdless <|
  let oldTrack = model.track in
  { model
  | track = { oldTrack | obstacles = oldTrack.obstacles ++ [obstacle] }
  }

setGameState : GameState -> Model -> Model
setGameState state model =
  let oldInput = model.input in
    { model
    | state = state
    , startMillis = model.millis
    , input = { oldInput | time = 0 }
    }

type alias Input =
  { jump: Bool
  , slide: Bool
  , keys: Set String
  , time: Float
  }


--------------------------------------------------------------------------------
-- Runner
--
-- Definition of the player character and its behaviour, i.e. input handling.
--------------------------------------------------------------------------------

{-| Possible runner states. -}
type RunnerState = Running | Jumping | Sliding

{-| Runner model. -}
type alias Runner =
  { state : RunnerState
  , y : Float
  , w : Float
  , h : Float
  , angle : Float
  , dimSnapshot : (Float, Float)
  , snapshotTime : Float
  }

{-| Default runner model instance. -}
initialRunner : Runner
initialRunner
  = {
    state = Running,
    y = 0,
    w = runnerSize,
    h = runnerSize,
    angle = 0,
    dimSnapshot = (runnerSize, runnerSize),
    snapshotTime = 0
  }

{-| Advance the runner a step forward according to received input. -}
runnerStep : Input -> Runner -> (Runner, Cmd Msg)
runnerStep input runner = cmdless <|
  let
    setState state =
      { runner
      | state = state
      , dimSnapshot = (runner.w, runner.h)
      , snapshotTime = input.time
      }
  in
    if input.jump && runner.state /= Jumping then
      setState Jumping
    else if input.slide && runner.state == Running then
      setState Sliding
    else if Set.isEmpty(input.keys) && runner.state == Sliding then
      setState Running
    else if runner.state == Jumping then
      jumpStep input runner
    else if runner.state == Sliding then
      slideStep input runner
    else
      runStep input runner

{-| A subroutine of `runnerStep`, called when no state transition has taken
place and the runner is in `Jumping` state. -}
jumpStep : Input -> Runner -> Runner
jumpStep input runner =
  let
    dt = input.time - runner.snapshotTime
    d = jumpDuration
    h = jumpHeight
    a = 4 * h / (d * d)
  in
    if d <= dt then
      { runner
      |  state = Running
      ,  y = 0
      ,  angle = 0
      ,  dimSnapshot = (runnerSize, runnerSize)
      ,  snapshotTime = input.time
      }
    else
      { runner
      | y = a * dt * (d - dt)
      , angle = dt / d * 90
      }
      |> dimTransition input dimTransitionDuration runnerSize runnerSize

{-| A subroutine of `runnerStep`, called when no state transition has taken
place and the runner is in `Sliding` state. -}
slideStep : Input -> Runner -> Runner
slideStep input =
  dimTransition input dimTransitionDuration (2 * runnerSize) (runnerSize / 2)

{-| A subroutine of `runnerStep`, called when no state transition has taken
place and the runner is in `Running` state. -}
runStep : Input -> Runner -> Runner
runStep input =
  dimTransition input dimTransitionDuration runnerSize runnerSize

{-| Animates the change in runner dimenstions. -}
dimTransition : Input -> Float -> Float -> Float -> Runner -> Runner
dimTransition input duration w h runner =
  let
    dt = input.time - runner.snapshotTime
    alpha = (min duration dt) / duration
    (w0, h0) = runner.dimSnapshot
  in
    { runner
    | w = lerp w0 w alpha
    , h = lerp h0 h alpha
    }

--------------------------------------------------------------------------------
-- Track
--
-- Definition of the track. The track consists of obstacles which are spwaned
-- at random.
--------------------------------------------------------------------------------

{-| Possible obstacle kinds. -}
type ObstacleKind = Spikes | Hole

{-| Obstacle model. -}
type alias Obstacle
  = {
    kind : ObstacleKind,
    spawnTime : Float,
    x : Float,
    units : Int
  }

{-| Track model. -}
type alias Track =
  { obstacles : List Obstacle
  }

{-| Default track model instance. -}
initialTrack : Track
initialTrack =
  { obstacles = []
  }

{-| Move obstacles toward the runner according to the elapsed time. -}
advanceObstacles : Input -> Track -> Track
advanceObstacles input track =
  let
    aux obstacle =
      { obstacle
      | x = canvasRight - (input.time - obstacle.spawnTime) * speed
      }
  in
    { track | obstacles = List.map aux track.obstacles }

{-| Remove the obstacles that are already past the left edge of the canvas. -}
dropInvisibleObstacles : Track -> Track
dropInvisibleObstacles track =
  let
    aux obstacle =
      obstacle.x + toFloat(obstacle.units) * obstacleUnit <= canvasLeft
  in
    { track | obstacles = dropWhile aux track.obstacles }

{-| Spawn a new obstacle if the previously spawned one is sufficiently far from
the right edge of the canvas. -}
maybeSpawnObstacle : Input -> Track -> (Track, Cmd Msg)
maybeSpawnObstacle input track = Tuple.pair track <|
  let
    x0 = canvasRight
    xMax = case last track.obstacles of
      Just obstacle -> obstacle.x + (toFloat obstacle.units) * obstacleUnit
      Nothing -> 0
  in
    if x0 - xMax >= minObstacleDistance then
      let
        t = input.time
        spawnTimeGenerator = Random.float t (t + minObstacleDistance / speed)
        obstackeKindGenerator = Random.weighted (3, Spikes) [(1, Hole)]
        unitGenerator kind =
          Random.int 1 <|
          case kind of
            Spikes -> maxSpikeUnits
            Hole -> maxHoleUnits
        obstacleGenerator =
          obstackeKindGenerator
          |> Random.andThen
            (\kind -> Random.map2
              (\spawnTime units ->
                { kind = kind
                , spawnTime = spawnTime
                , x = x0
                , units = units 
                }
              )
              spawnTimeGenerator
              (unitGenerator kind)
            )
      in
        Random.generate SpawnObstacle obstacleGenerator
    else
      Cmd.none

{-| Advance the track by a single step according to received input. -}
trackStep : Input -> Track -> (Track, Cmd Msg)
trackStep input track =
  track
  |> advanceObstacles input
  |> dropInvisibleObstacles
  |> maybeSpawnObstacle input

--------------------------------------------------------------------------------
-- Model
--
-- The complete model of the application. Contains the runner and the track.
-- Keeps track of the score and failure condition as well.
--------------------------------------------------------------------------------

{-| Pissible states of the application. -}
type GameState = Paused | Playing

{-| Application model. -}
type alias Model =
  { input : Input
  , state : GameState
  , runner : Runner
  , track : Track
  , millis : Int
  , startMillis : Int
  , score : Int
  }

{-| Default application model instance. -}
initialModel =
  { state = Paused
  , input =
    { jump = False
    , slide = False
    , keys = Set.empty
    , time = 0
    }
  , runner = initialRunner
  , track = initialTrack
  , millis = 0
  , startMillis = 0
  , score = 0
  }

{-| Advance the game by a single step according to received input. -}
step : Model -> (Model, Cmd Msg)
step model =
  case model.state of
    Paused -> pausedStep model
    Playing -> playingStep model

{-| Subroutine of `step`, called when the game is in the `Paused` state. -}
pausedStep : Model -> (Model, Cmd Msg)
pausedStep model = cmdless <|
  if Set.isEmpty(model.input.keys) || model.input.time < pauseMargin then
    model
  else
    { model
    | track = initialTrack
    , runner = initialRunner
    , score = 0
    }
    |> setGameState Playing

{-| Subroutine of `step`, called when the game is in the `Playing` state. -}
playingStep : Model -> (Model, Cmd Msg)
playingStep model =
  let
    (runner, runnerCmd) = runnerStep model.input model.runner
    (track, trackCmd) = trackStep model.input model.track
  in
    ( { model
      | runner = runner
      , track = track
      , score = floor (model.input.time * 10)
      }
      |> collisions
    , Cmd.batch [runnerCmd, trackCmd]
    )

{-| Check wether the runner collides with an obstacle. -}
collides : Runner -> Obstacle -> Bool
collides runner obstacle =
  let
    x0 = obstacle.x - runner.w / 2 + obstacleMargin
    x1 = obstacle.x + toFloat(obstacle.units) * obstacleUnit
       + runner.w / 2 - obstacleMargin
    x = 0
    y = runner.y
    h = runner.h
  in case obstacle.kind of
    Spikes -> y < spikeH - obstacleMargin && y < -sqrt3 * x0 && y < sqrt3 * x1
    Hole -> x0 < 0 && 0 < x1 && h > runnerSize / 2

{-| Handle failure contition. If the runner collides with any obstacle, returns
to the title screen. -}
collisions : Model -> Model
collisions model =
  if any (collides model.runner) model.track.obstacles then
    setGameState Paused model
  else
    model

init : () -> (Model, Cmd Msg)
init _ = (initialModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Key state key -> applyKey state key model
    Touch touch -> applyTouch touch model
    Tick posix -> applyTick posix model
    SpawnObstacle obstacle -> applySpawnObstacle obstacle model

port onTouch : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onKeyDown (keyDecoder True)
    , Browser.Events.onKeyUp (keyDecoder False)
    , Browser.Events.onAnimationFrame Tick
    , onTouch Touch
    ]

--------------------------------------------------------------------------------
-- Display
--
-- All drawing routines.
--------------------------------------------------------------------------------

{-| Formats a `rotate` transform attribtue. -}
rotate : Float -> String
rotate angle = "rotate(" ++ String.fromFloat angle ++ ")"

{-| Formats a `translate` transform attribtue. -}
translate : Float -> Float -> String
translate x y
  = "translate("
  ++ String.fromFloat x
  ++ ","
  ++ String.fromFloat y
  ++ ")"

{-| Formats a list of points for an SVG polygon. -}
points : List (Float, Float) -> String
points ps =
  List.map (\(x, y) -> String.fromFloat x ++ "," ++ String.fromFloat y) ps
  |> String.join " "

{-| Produce an `Svg` representation of the runner. -}
runnerView : Runner -> Svg Msg
runnerView runner =
  Svg.g
    [ Svg.Attributes.transform (translate runnerX (canvasHeight - runner.y - runner.h / 2))
    ]
    [ Svg.rect
        [ Svg.Attributes.x (String.fromFloat (-runner.w / 2))
        , Svg.Attributes.y (String.fromFloat (-runner.h / 2))
        , Svg.Attributes.width (String.fromFloat runner.w)
        , Svg.Attributes.height (String.fromFloat runner.h)
        , Svg.Attributes.transform (rotate runner.angle)
        ]
        []
    ]

{-| Produce an `Svg` representation of an obstacle. -}
obstacleView : Obstacle -> Svg Msg
obstacleView obstacle = case obstacle.kind of
  Spikes ->
    let
      spikeView i =
        let x0 = toFloat(i) * obstacleUnit in
        Svg.polygon
          [ Svg.Attributes.points (points
              [ (x0, 0)
              , (x0 + obstacleUnit / 2, -spikeH)
              , (x0 + obstacleUnit, 0)
              ])
          ]
          []
    in
      Svg.g
        [ Svg.Attributes.transform (translate (runnerX + obstacle.x) canvasHeight) ]
        (iter obstacle.units spikeView)
  Hole -> Svg.rect
    [ Svg.Attributes.x (String.fromFloat (runnerX + obstacle.x))
    , Svg.Attributes.y "0"
    , Svg.Attributes.width (String.fromFloat (toFloat obstacle.units * obstacleUnit))
    , Svg.Attributes.height (String.fromFloat (canvasHeight - runnerSize / 2))
    ]
    []

--{-| Produce an `Svg` representation of the track. -}
trackView : Track -> Svg Msg
trackView track = Svg.g [] (List.map obstacleView track.obstacles)

--{-| Produce the title screen `Html`. -}
pausedView =
  Html.div
  [ Html.Attributes.style "display" "flex"
  , Html.Attributes.style "flex-direction" "column"
  , Html.Attributes.style "justify-content" "center"
  , Html.Attributes.style "align-items" "center"
  , Html.Attributes.style "width" "100%"
  , Html.Attributes.style "height" "100%"
  , Html.Attributes.style "color" fg
  ]
  [ Html.div
      [ Html.Attributes.style "font-size" "9vw"]
      [ Html.text "Elm Runner" ]
  , Html.div
      [ Html.Attributes.style "font-size" "2.5vw"
      , Html.Attributes.style "text-align" "center"
      ]
      [ Html.text "KEY UP or TOUCH RIGHT to jump"
      , Html.br [] []
      , Html.text "KEY DOWN or TOUCH LEFT  to crouch"
      , Html.br [] []
      , Html.text "Press anything to start"
      ]
  ]

--{-| Produce an `Svg` representation of the whole scene. -}
playingView : Model -> List (Svg Msg)
playingView model =
  [ runnerView model.runner
  , trackView model.track
  ]

view : Model -> Html Msg
view model =
  Html.div
    [ Html.Attributes.style "pointer-events" "none"
    , Html.Attributes.style "user-select" "none"
    , Html.Attributes.style "-webkit-user-select" "none"
    , Html.Attributes.style "-moz-user-select" "none"
    , Html.Attributes.style "-ms-user-select" "none"
    , Html.Attributes.style "-khtml-user-select" "none"
    ]
    [ Html.div
        [ Html.Attributes.style "color" bg
        , Html.Attributes.style "font-size" "3vw"
        , Html.Attributes.style "margin" "1vw"
        ]
        [ Html.text ("Score: " ++ (String.fromInt model.score)) ]
    , Html.div
        [ Html.Attributes.style "background-color" bg
        , Html.Attributes.style "position" "relative"
        , Html.Attributes.style "margin-bottom" "6vw"
        ]
        [ svg
            [ Svg.Attributes.display "block"
            , Svg.Attributes.width "100%"
            , Svg.Attributes.viewBox
                ("0 0 "
                ++ (String.fromInt canvasWidth)
                ++ " "
                ++ (String.fromInt canvasHeight)
                )
            , Svg.Attributes.fill fg
            ]
            (if model.state == Playing then playingView model else [])
        , Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "right" "0"
            ]
            (if model.state == Paused then [pausedView] else [])
        ]
      ]

{-| The entry point of the application -}
--main = display <~ Window.dimensions ~ foldp step initialModel input
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
