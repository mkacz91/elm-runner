import Color exposing (darkBlue, white)
import Graphics.Element exposing (Element, container, middle)
import Graphics.Collage exposing (Form, collage, rect, ngon, filled, move, rotate, group)
import Keyboard exposing (KeyCode)
import List exposing (any)
import Random exposing (Generator, Seed, customGenerator, generate, initialSeed)
import Set exposing (Set, member)
import Signal exposing ((<~), (~), sampleOn, foldp)
import Time exposing (Time, fps, inSeconds)
import Window

--------------------------------------------------------------------------------
-- Configuration                                                              --
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
minObstacleDistance = 100.0
obstacleMargin = 5
pauseMargin = 1.0

canvasLeft = -canvasWidth / 2 - runnerX
canvasRight = canvasWidth / 2 - runnerX
sqrt3 = sqrt(3)
spikeH = sqrt3 / 2 * obstacleUnit

port randomSeed : Int

--------------------------------------------------------------------------------
-- Utils                                                                      --
--------------------------------------------------------------------------------

lerp : Float -> Float -> Float -> Float
lerp x y alpha = (1 - alpha) * x + alpha * y

iter : Int -> (Int -> a) -> List a
iter n f =
  let aux n xs = if n < 1 then xs else aux (n - 1) (f (n - 1) :: xs)
  in aux n []

dropWhile : (a -> Bool) -> List a -> List a
dropWhile f xs = case xs of
  x :: xs' -> if f x then dropWhile f xs' else xs
  [] -> []

last : List a -> Maybe a
last xs = case xs of
  [] -> Nothing
  [x] -> Just x
  _ :: xs' -> last xs'

setIsEmpty : Set comparable -> Bool
setIsEmpty xs = (Set.foldl (\_ s -> s + 1) 0 xs) == 0

--------------------------------------------------------------------------------
-- Input                                                                      --
--------------------------------------------------------------------------------

-- Keyboard input

type Key = Jump | Slide | Some | None

resolveKeys : Set KeyCode -> Key
resolveKeys keysDown =
  case (member 38 keysDown, member 40 keysDown) of
    (True, False) -> Jump
    (False, True) -> Slide
    _             -> if setIsEmpty keysDown then None else Some

key : Signal Key
key = Signal.map resolveKeys Keyboard.keysDown

-- Time input

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
-- Runner                                                                     --
--------------------------------------------------------------------------------

type RunnerState = Running | Jumping | Sliding

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
-- Track                                                                      --
--------------------------------------------------------------------------------

type ObstacleKind = Spikes | Hole

type alias Obstacle
  = {
    kind : ObstacleKind,
    spawnTime : Float,
    x : Float,
    units : Int
  }

type alias Track
  = {
    obstacles : List Obstacle,
    seed : Seed
  }

initialTrack : Track
initialTrack
  = {
    obstacles = [],
    seed = initialSeed randomSeed
  }

obstacleKindGenerator : Generator ObstacleKind
obstacleKindGenerator =
  let
    auxGenerator = Random.int 0 1
    aux seed = case generate (Random.int 0 1) seed of
      (0, seed') -> (Spikes, seed')
      (1, seed') -> (Hole, seed')
  in
    customGenerator aux

advanceObstacles : Input -> Track -> Track
advanceObstacles input track =
  let
    x0 = canvasRight
    aux obstacle =
      { obstacle | x <- x0 - (input.time - obstacle.spawnTime) * speed }
  in
    { track | obstacles <- List.map aux track.obstacles }

dropInvisibleObstacles : Track -> Track
dropInvisibleObstacles track =
  let aux obstacle =
    obstacle.x + toFloat(obstacle.units) * obstacleUnit <= canvasLeft
  in
    { track | obstacles <- dropWhile aux track.obstacles }

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
          Spikes -> generate (Random.int 1 3) seed2
          Hole -> generate (Random.int 1 8) seed2
        obstacle
          = {
            kind = kind,
            spawnTime = t,
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


trackStep : Input -> Track -> Track
trackStep input track =
  track
  |> advanceObstacles input
  |> dropInvisibleObstacles
  |> maybeSpawnObstacle input

--------------------------------------------------------------------------------
-- Model                                                                      --
--------------------------------------------------------------------------------

type GameState = Paused Float | Playing

type alias Model
  = {
    state : GameState,
    runner : Runner,
    track : Track,
    score : Int
  }

initialModel
  = {
    state = Paused 0,
    runner = initialRunner,
    track = initialTrack,
    score = 0
  }

step : Input -> Model -> Model
step input model =
  case model.state of
    Paused at -> pausedStep input at model
    Playing -> playingStep input model

pausedStep : Input -> Float -> Model -> Model
pausedStep input pauseTime model =
  if input.key == None || input.time < pauseTime + pauseMargin then model
  else let track = model.track in
    { model |
      state <- Playing,
      track <- { track | obstacles <- [] },
      score <- 0
    }

playingStep : Input -> Model -> Model
playingStep input model
  = { model |
    runner <- runnerStep input model.runner,
    track <- trackStep input model.track,
    score <- floor (input.time * 10)
  }
  |> collisions input

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

collisions : Input -> Model -> Model
collisions input model =
  if any (collides model.runner) model.track.obstacles then
    { model | state <- Paused input.time }
  else
    model

--------------------------------------------------------------------------------
-- Display                                                                    --
--------------------------------------------------------------------------------

runnerForm : Runner -> Form
runnerForm runner =
  rect runner.w runner.h
  |> filled fg
  |> rotate runner.angle
  |> move (0, runner.y + runner.h / 2)

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

trackForm : Track -> Form
trackForm track = group (List.map obstacleForm track.obstacles)

pausedForm : Model -> Form
pausedForm mode = rect 10 10 |> filled fg

playingForm : Model -> Form
playingForm model =
  group [runnerForm model.runner, trackForm model.track]
  |> move (runnerX, -canvasHeight / 2)

display : (Int, Int) -> Model -> Element
display (w, h) model =
  container w h middle (
    collage canvasWidth canvasHeight [
      rect canvasWidth canvasHeight |> filled bg,
      case model.state of
        Paused _ -> pausedForm model
        Playing -> playingForm model
    ]
  )

main = display <~ Window.dimensions ~ foldp step initialModel input
