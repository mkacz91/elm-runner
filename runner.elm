import Color exposing (darkBlue, white)
import Graphics.Element exposing (Element, container, middle, centered, leftAligned, above)
import Graphics.Collage exposing (Form, collage, rect, ngon, filled, move, rotate, group, toForm)
import Keyboard exposing (KeyCode)
import List exposing (any)
import Random exposing (Generator, Seed, customGenerator, generate, initialSeed)
import Set exposing (Set, member)
import Signal exposing ((<~), (~), foldp)
import Text
import Time exposing (Time, fps, inSeconds)
import Window

--------------------------------------------------------------------------------
-- Configuration
--
-- This part contains configuration constants that determine the appearance,
-- behaviour, and, inconsequence, the difficulty.
--------------------------------------------------------------------------------

canvasWidth = 600
canvasHeight = 120
bg = darkBlue
fg = white

runnerSize = 20.0
runnerX = -100.0
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

canvasLeft = -canvasWidth / 2 - runnerX
canvasRight = canvasWidth / 2 - runnerX
sqrt3 = sqrt(3)
spikeH = sqrt3 / 2 * obstacleUnit

-- Ported constants

port randomSeed : Int

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
  let aux n xs = if n < 1 then xs else aux (n - 1) (f (n - 1) :: xs)
  in aux n []

{-| Drops all initial elements of a list that fulfill a predicate. -}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile f xs = case xs of
  x :: xs' -> if f x then dropWhile f xs' else xs
  [] -> []

{-| Retrieve the last element of a list. -}
last : List a -> Maybe a
last xs = case xs of
  [] -> Nothing
  [x] -> Just x
  _ :: xs' -> last xs'

{-| Check whether a set is empty. -}
setIsEmpty : Set comparable -> Bool
setIsEmpty xs = (Set.foldl (\_ s -> s + 1) 0 xs) == 0

--------------------------------------------------------------------------------
-- Input
--
-- Keyboard and time signals
--------------------------------------------------------------------------------

-- Keyboard input

{-| Supported keys. -}
type Key = Jump | Slide | Some | None

{-| Transform raw keyboard data into `Key`. -}
resolveKeys : Set KeyCode -> Key
resolveKeys keysDown =
  case (member 38 keysDown, member 40 keysDown) of
    (True, False) -> Jump
    (False, True) -> Slide
    _             -> if setIsEmpty keysDown then None else Some

{-| Signal carrying the pressed `Key`. -}
key : Signal Key
key = Signal.map resolveKeys Keyboard.keysDown

-- Time input

{-| Signal carrying the time elapsed since the start of application. Updated
60 times per second. -}
time : Signal Float
time = foldp (+) 0 (inSeconds <~ fps 60)

-- Merged input

type alias Input
  = {
    key : Key,
    time : Float
  }

input : Signal Input
input = Input <~ key ~ time

--------------------------------------------------------------------------------
-- Runner
--
-- Definition of the player character and its behaviour, i.e. input handling.
--------------------------------------------------------------------------------

{-| Possible runner states. -}
type RunnerState = Running | Jumping | Sliding

{-| Runner model. -}
type alias Runner
  = {
    state : RunnerState,
    y : Float,
    w : Float,
    h : Float,
    angle : Float,
    dimSnapshot : (Float, Float),
    snapshotTime : Float
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
runnerStep : Input -> Runner -> Runner
runnerStep input runner =
  let setState state
    = { runner |
      state <- state,
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
      { runner |
        state <- Running,
        y <- 0,
        angle <- 0,
        dimSnapshot <- (runnerSize, runnerSize),
        snapshotTime <- input.time
      }
    else
      { runner |
        y <- a * dt * (d - dt),
        angle <- -dt / d * pi / 2
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
    { runner |
      w <- lerp w0 w alpha,
      h <- lerp h0 h alpha
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
type alias Track
  = {
    obstacles : List Obstacle,
    seed : Seed
  }

{-| Default track model instance. -}
initialTrack : Track
initialTrack
  = {
    obstacles = [],
    seed = initialSeed randomSeed
  }

{-| A random generator yielding `Spikes` with probility 3/4 and `Hole` with
probability 1/4. -}
obstacleKindGenerator : Generator ObstacleKind
obstacleKindGenerator =
  let aux seed =
    case generate (Random.int 0 3) seed of
      (0, seed') -> (Hole, seed')
      (_, seed') -> (Spikes, seed')
  in customGenerator aux

{-| Move obstacles toward the runner according to the elapsed time. -}
advanceObstacles : Input -> Track -> Track
advanceObstacles input track =
  let
    x0 = canvasRight
    aux obstacle =
      { obstacle | x <- x0 - (input.time - obstacle.spawnTime) * speed }
  in
    { track | obstacles <- List.map aux track.obstacles }

{-| Remove the obstacles that are already past the left edge of the canvas. -}
dropInvisibleObstacles : Track -> Track
dropInvisibleObstacles track =
  let aux obstacle =
    obstacle.x + toFloat(obstacle.units) * obstacleUnit <= canvasLeft
  in
    { track | obstacles <- dropWhile aux track.obstacles }

{-| Spawn a new obstacle if the previously spawned one is sufficiently far from
the right edge of the canvas. -}
maybeSpawnObstacle : Input -> Track -> Track
maybeSpawnObstacle input track =
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
        (kind, seed1) = generate obstacleKindGenerator track.seed
        (spawnTime, seed2) = generate spawnTimeGenerator seed1
        (units, seed3) = case kind of
          Spikes -> generate (Random.int 1 maxSpikeUnits) seed2
          Hole -> generate (Random.int 1 maxHoleUnits) seed2
        obstacle
          = {
            kind = kind,
            spawnTime = spawnTime,
            x = x0,
            units = units
          }
      in
        { track |
          obstacles <- track.obstacles ++ [obstacle],
          seed <- seed3
        }
    else
      track

{-| Advance the track by a single step according to received input. -}
trackStep : Input -> Track -> Track
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
type alias Model
  = {
    state : GameState,
    runner : Runner,
    track : Track,
    startTime : Float,
    score : Int
  }

{-| Default application model instance. -}
initialModel
  = {
    state = Paused,
    runner = initialRunner,
    track = initialTrack,
    startTime = 0,
    score = 0
  }

{-| Advance the game by a single step according to received input. -}
step : Input -> Model -> Model
step input model =
  case model.state of
    Paused -> pausedStep input model
    Playing -> playingStep input model

{-| Subroutine of `step`, called when the game is in the `Paused` state. -}
pausedStep : Input -> Model -> Model
pausedStep input model =
  if input.key == None || input.time < model.startTime + pauseMargin then model
  else let track = model.track in
    { model |
      state <- Playing,
      track <- { track | obstacles <- [] },
      startTime <- input.time,
      score <- 0
    }

{-| Subroutine of `step`, called when the game is in the `Playing` state. -}
playingStep : Input -> Model -> Model
playingStep input model
  = { model |
    runner <- runnerStep input model.runner,
    track <- trackStep input model.track,
    score <- floor ((input.time - model.startTime) * 10)
  }
  |> collisions input

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
collisions : Input -> Model -> Model
collisions input model =
  if any (collides model.runner) model.track.obstacles then
    { model |
      state <- Paused,
      startTime <- input.time }
  else
    model

--------------------------------------------------------------------------------
-- Display
--
-- All drawing routines.
--------------------------------------------------------------------------------

{-| Produce a `Graphics.Collage.Form` representation of the runner. -}
runnerForm : Runner -> Form
runnerForm runner =
  rect runner.w runner.h
  |> filled fg
  |> rotate runner.angle
  |> move (0, runner.y + runner.h / 2)

{-| Produce a `Graphics.Collage.Form` representation of an obstacle. -}
obstacleForm : Obstacle -> Form
obstacleForm obstacle = case obstacle.kind of
  Spikes ->
    let
      radius = 2 * spikeH / 3
      spikeForm i =
        ngon 3 radius
        |> filled fg
        |> rotate (pi / 2)
        |> move (toFloat(i) * obstacleUnit, 0)
    in
      iter obstacle.units spikeForm
      |> group
      |> move (obstacle.x + obstacleUnit / 2, spikeH / 3)
  Hole ->
    let
      w = toFloat(obstacle.units) * obstacleUnit
      h = canvasHeight - runnerSize / 2
    in
      rect w h
      |> filled fg
      |> move (obstacle.x + w / 2, h / 2 + runnerSize / 2)

{-| Produce a `Graphics.Collage.Form` representation of the track. -}
trackForm : Track -> Form
trackForm track = group (List.map obstacleForm track.obstacles)

{-| Produce the title screen form. -}
pausedForm : Model -> Form
pausedForm model =
  group [
    Text.fromString "Elm Runner"
    |> Text.color fg
    |> Text.height 40
    |> Text.monospace
    |> centered
    |> toForm
    |> move (0, 20),
    Text.fromString "UP to jump, DOWN to crouch\nAny key to start"
    |> Text.color fg
    |> Text.height 12
    |> Text.monospace
    |> centered
    |> toForm
    |> move (0, -20)
  ]

{-| Produce a `Graphics.Collage.Form` representation of the whole scene. -}
playingForm : Model -> Form
playingForm model =
  group [runnerForm model.runner, trackForm model.track]
  |> move (runnerX, -canvasHeight / 2)

{-| Produce a `Graphics.Element` representation of the score. -}
scoreElement : Model -> Element
scoreElement model =
  Text.fromString ("Score: " ++ toString model.score)
  |> Text.color bg
  |> Text.height 12
  |> Text.monospace
  |> leftAligned
  |> Graphics.Element.color fg

{-| Display the application as a `Graphics.Element`. -}
display : (Int, Int) -> Model -> Element
display (w, h) model =
  container w h middle (
    scoreElement model
    `above`
    collage canvasWidth canvasHeight [
      rect canvasWidth canvasHeight |> filled bg,
      case model.state of
        Paused -> pausedForm model
        Playing -> playingForm model
    ]
  )

{-| The entry point of the application -}
main = display <~ Window.dimensions ~ foldp step initialModel input
