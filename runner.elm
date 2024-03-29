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

version = "v0.2.5"

canvasWidth = 30
canvasHeight = 6

runnerX = 9.0
jumpHeight = 2.5
jumpDistance = 6.0
dimTransitionDistance = 1.0
speed = 10.0
obstacleUnit = 1.5
obstacleMargin = 0.25
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

{-| Applies a function to a value if a condition is met. -}
applyIf : Bool -> (a -> a) -> a -> a
applyIf cond f x = if cond then f x else x

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
  | HighScore Int

applyTick : Time.Posix -> Model -> (Model, Cmd Msg)
applyTick posix model =
  let
    oldInput = model.input
    wallMillis = posixToMillis posix
    dWallMillis = wallMillis - model.wallMillis
    millis = model.millis + min dWallMillis 66 -- Throttle below 15 fps.
    input =
      { oldInput
      | distance = (toFloat millis) * speed / 1000.0
      }
  in
    { model
    | wallMillis = wallMillis
    , millis = millis
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
  | track =
    { oldTrack
    | obstacles = oldTrack.obstacles ++ [obstacle]
    , nonHoleStride = if obstacle.kind /= Hole
        then oldTrack.nonHoleStride + 1
        else 0
    }
  }

applyHighScore : Int -> Model -> (Model, Cmd Msg)
applyHighScore score model = cmdless <| { model | highScore = score }

setGameState : GameState -> Model -> Model
setGameState state model =
  let oldInput = model.input in
    { model
    | state = state
    , millis = 0
    , input = { oldInput | distance = 0 }
    }

type alias Input =
  { jump: Bool
  , slide: Bool
  , keys: Set String
  , distance: Float
  }

port announceVersion : String -> Cmd msg
port announceScore : Int -> Cmd msg
port onHighScore : (Int -> msg) -> Sub msg

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
  , angleSnapshot : Float
  , dimSnapshot : (Float, Float)
  , snapshotDistance : Float
  , pfpUrl : Maybe String
  }

{-| Default runner model instance. -}
initialRunner : InitFlags -> Runner
initialRunner flags =
  { state = Running
  , y = 0.0
  , w = 1.0
  , h = 1.0
  , angle = 0.0
  , angleSnapshot = 0.0
  , dimSnapshot = (1.0, 1.0)
  , snapshotDistance = 0
  , pfpUrl = flags.pfpUrl
  }

setRunnerState : Input -> RunnerState -> Runner -> Runner
setRunnerState input state runner =
  { runner
  | state = state
  , angleSnapshot = if runner.angle < 360.0 then runner.angle else 0.0
  , dimSnapshot = (runner.w, runner.h)
  , snapshotDistance = input.distance
  }

{-| Advance the runner a step forward according to received input. -}
runnerStep : Input -> Runner -> (Runner, Cmd Msg)
runnerStep input runner =
  cmdless <|
  dimTransition input <|
  if input.jump && runner.state /= Jumping then
    setRunnerState input Jumping runner
  else if input.slide && runner.state == Running then
    setRunnerState input Sliding runner
  else if Set.isEmpty(input.keys) && runner.state == Sliding then
    setRunnerState input Running runner
  else if runner.state == Jumping then
    jumpStep input runner
  else runner

{-| A subroutine of `runnerStep`, called when no state transition has taken
place and the runner is in `Jumping` state. -}
jumpStep : Input -> Runner -> Runner
jumpStep input runner =
  let
    ds = input.distance - runner.snapshotDistance
    d = jumpDistance
    h = jumpHeight
    a = 4 * h / (d * d)
  in
    if d <= ds then
      { runner
      | y = 0
      , angle = runner.angleSnapshot + 90.0
      }
      |> setRunnerState input Running
    else
      { runner
      | y = a * ds * (d - ds)
      , angle = runner.angleSnapshot + ds / d * 90.0
      }

{-| Animates the change in runner dimenstions. -}
dimTransition : Input -> Runner -> Runner
dimTransition input runner =
  let
    h1 = case runner.state of
      Running -> 1.0
      Jumping -> 1.0
      Sliding -> 0.5
    w1 = 1.0 / h1
    deviation = (abs (runner.h - h1)) / 0.5
    requiredDistance = dimTransitionDistance * deviation
    ds = input.distance - runner.snapshotDistance
    (w0, h0) = runner.dimSnapshot
  in
    if ds >= requiredDistance then
      { runner
      | w = w1
      , h = h1
      }
    else
      let alpha = ds / requiredDistance in
      { runner
      | w = lerp w0 w1 alpha
      , h = lerp h0 h1 alpha
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
    spawnDistance : Float,
    x : Float,
    units : Int
  }

{-| Track model. -}
type alias Track =
  { obstacles : List Obstacle
  , nonHoleStride : Int
  }

{-| Default track model instance. -}
initialTrack : Track
initialTrack =
  { obstacles = []
  , nonHoleStride = 0
  }

{-| Move obstacles toward the runner. -}
advanceObstacles : Input -> Track -> Track
advanceObstacles input track =
  let
    aux obstacle =
      { obstacle
      | x = canvasRight - (input.distance - obstacle.spawnDistance)
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

optimalClearX : Obstacle -> Float -> Float
optimalClearX obstacle x0=
  let
    obstacleW = toFloat obstacle.units * obstacleUnit
    x1 = obstacle.x + obstacleW - obstacleMargin + 0.5
  in case obstacle.kind of
    Hole -> x1 + 2.0
    Spikes -> max x1 (x0 + jumpDistance)

{-| Spawn a new obstacle if the previously spawned one is sufficiently far from
the right edge of the canvas. -}
maybeSpawnObstacle : Input -> Track -> (Track, Cmd Msg)
maybeSpawnObstacle input track = Tuple.pair track <|
  let xc = List.foldl optimalClearX 0 track.obstacles
  in if xc > canvasRight then Cmd.none else
  let
    s = input.distance
    is = floor s
    -- Asymptotic fall from 5 to 1.7. Around 3.7 at distance 500.
    gap = (5.0 - 1.7) * e^(-s / 1000.0) + 1.7
    spawnDistanceGenerator = Random.float (s + gap) (s + 2 * gap)
    -- Linear fall from 5 by 1 every 500 units. Not under 1.
    requiredNonHoleStride = max 1 (ceiling (4 - s / 500.0))
    -- Asymptotic rise from 0.3 to 0.95. Around 0.5 at distance 1000, 0.7 at 3k.  
    allowedHoleProbability = 0.95 - 0.65 / (s / 1800.0 + 1.0)
    holeProbability = if track.nonHoleStride >= requiredNonHoleStride
      then allowedHoleProbability
      else 0
    obstackeKindGenerator = Random.weighted
      (1.0 - holeProbability, Spikes)
      [(holeProbability, Hole)]
    unitGenerator kind =
      case kind of
        Hole -> Random.int 1 maxHoleUnits
        Spikes ->
          let
            w1 = toFloat <| 1 + clamp 0 2 (ceiling ((s - 1100) / 300.0))
            w2 = toFloat <| clamp 0 2 (floor (s / 300.0))
            w3 = toFloat <| clamp 0 4 (ceiling ((s - 800.0) / 300.0))
          in
            Random.weighted (w1, 1) [(w2, 2), (w3, 3)]
    obstacleGenerator =
      obstackeKindGenerator
      |> Random.andThen
        (\kind -> Random.map2
          (\spawnDistance units ->
            { kind = kind
            , spawnDistance = spawnDistance
            , x = canvasRight + spawnDistance - s
            , units = units 
            }
          )
          spawnDistanceGenerator
          (unitGenerator kind)
        )
  in
    Random.generate SpawnObstacle obstacleGenerator

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

{-| Color theme. -}
type alias Theme =
  { bg : String
  , fg : String
  }

themes : List Theme
themes =
  [ { bg = "#204a87", fg = "#ffffff" } -- 0
  , { bg = "#ffffff", fg = "#204a87" } -- 400
  , { bg = "#ffffff", fg = "#208733" } -- 800
  , { bg = "#208733", fg = "#ffffff" } -- 1200
  , { bg = "#872046", fg = "#ffffff" } -- 1600
  , { bg = "#ffffff", fg = "#872046" } -- 2000
  , { bg = "#ffffff", fg = "#f5a70c" } -- 2400
  , { bg = "#f5a70c", fg = "#ffffff" } -- 2800
  , { bg = "#0c71f5", fg = "#ffffff" } -- 3200
  , { bg = "#ffffff", fg = "#0c71f5" } -- 3600
  ]

{-| Application model. -}
type alias Model =
  { input : Input
  , state : GameState
  , initialRunner : Runner
  , runner : Runner
  , track : Track
  , wallMillis : Int
  , millis : Int
  , score : Int
  , highScore : Int
  , themeQueue: List Theme
  , theme: Theme
  , themeIndex: Int
  }

type alias InitFlags =
  { pfpUrl : Maybe String
  }

sanitizePfpUrl : String -> String
sanitizePfpUrl url =
  if String.startsWith "data:" url then
    url
    |> String.replace "-" "+"
    |> String.replace "_" "/"
  else
    url

{-| Default application model instance. -}
initialModel : InitFlags -> Model
initialModel flags =
  let
    runner = initialRunner flags
  in
    { state = Paused
    , input =
      { jump = False
      , slide = False
      , keys = Set.empty
      , distance = 0
      }
    , initialRunner = runner
    , runner = runner
    , track = initialTrack
    , wallMillis = 0
    , millis = 0
    , score = 0
    , highScore = 0
    , themeQueue = []
    , theme = { bg = "magenta", fg = "cyan" }
    , themeIndex = 0
    }
    |> resetTheme

{-| Advance the game by a single step according to received input. -}
step : Model -> (Model, Cmd Msg)
step model =
  case model.state of
    Paused -> pausedStep model
    Playing -> playingStep model

{-| Subroutine of `step`, called when the game is in the `Paused` state. -}
pausedStep : Model -> (Model, Cmd Msg)
pausedStep model = cmdless <|
  if Set.isEmpty(model.input.keys) || model.millis < 1000 then
    model
  else
    { model
    | track = initialTrack
    , runner = model.initialRunner
    , score = 0
    }
    |> resetTheme
    |> setGameState Playing

{-| Subroutine of `step`, called when the game is in the `Playing` state. -}
playingStep : Model -> (Model, Cmd Msg)
playingStep model =
  let
    wasJumping = model.runner.state == Jumping
    (runner, runnerCmd) = runnerStep model.input model.runner
    justLanded = wasJumping && runner.state /= Jumping
    (track, trackCmd) = trackStep model.input model.track
    score = floor model.input.distance
    shouldActivateNextFrame = justLanded && score // 400 > model.themeIndex
  in
    ( { model
      | runner = runner
      , track = track
      , score = score
      }
      |> applyIf shouldActivateNextFrame activateNextTheme
      |> collisions
    , Cmd.batch [runnerCmd, trackCmd, announceScore score]
    )

{-| Check wether the runner collides with an obstacle. -}
collides : Runner -> Input -> Obstacle -> Bool
collides runner input obstacle =
  let
    x0 = obstacle.x - runner.w / 2 + obstacleMargin
    x1 = obstacle.x + toFloat obstacle.units * obstacleUnit
      + 0.5 - obstacleMargin
    overlapsX x = x0 < x && x < x1
  in case obstacle.kind of
    Hole -> overlapsX 0 && runner.h > 0.5
    Spikes ->
      if runner.state /= Jumping then
        overlapsX 0
      else if input.distance >= runner.snapshotDistance + 0.5 * jumpDistance then
        overlapsX (jumpDistance - input.distance + runner.snapshotDistance)
          && runner.y < spikeH - obstacleMargin
      else
        False

{-| Handle failure contition. If the runner collides with any obstacle, returns
to the title screen. -}
collisions : Model -> Model
collisions model =
  if any (collides model.runner model.input) model.track.obstacles then
    setGameState Paused model
  else
    model

resetTheme : Model -> Model
resetTheme model =
  { model
  | themeQueue = []
  , theme = { bg = "magenta", fg = "cyan" }
  , themeIndex = -1
  }
  |> activateNextTheme

activateNextTheme : Model -> Model
activateNextTheme model =
  case model.themeQueue of
    [] -> activateNextTheme { model | themeQueue = themes }
    t :: ts ->
      { model
      | themeQueue = ts
      , theme = t
      , themeIndex = model.themeIndex + 1 
      }

init : InitFlags -> (Model, Cmd Msg)
init flags =
  let
    sanitizedFlags =
      { flags
      | pfpUrl = Maybe.map sanitizePfpUrl flags.pfpUrl
      }
  in
    (initialModel sanitizedFlags, announceVersion version)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Key state key -> applyKey state key model
    Touch touch -> applyTouch touch model
    Tick posix -> applyTick posix model
    SpawnObstacle obstacle -> applySpawnObstacle obstacle model
    HighScore score -> applyHighScore score model

port onTouch : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onKeyDown (keyDecoder True)
    , Browser.Events.onKeyUp (keyDecoder False)
    , Browser.Events.onAnimationFrame Tick
    , onTouch Touch
    , onHighScore HighScore
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

scale : Float -> Float -> String
scale x y
  = "scale("
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
    [ Svg.Attributes.transform
        ( (translate runnerX (canvasHeight - runner.y - runner.h / 2))
        ++ " " ++ (scale runner.w runner.h)
        ++ " " ++ (rotate runner.angle)
        )
    ]
    (
      (
        case runner.pfpUrl of
          Nothing -> []
          Just pfpUrl -> 
            [ Svg.defs
              []
              [ Svg.filter
                  [ Svg.Attributes.id "pfp"]
                  [ Svg.feImage
                      [ Html.Attributes.attribute "href" pfpUrl
                      , Svg.Attributes.x "-0.5"
                      , Svg.Attributes.y "-0.5"
                      , Svg.Attributes.width "1"
                      , Svg.Attributes.height "1"
                      , Svg.Attributes.result "r0"
                      , Svg.Attributes.preserveAspectRatio "none"
                      ]
                      []
                  , Svg.feColorMatrix
                      [ Svg.Attributes.in_ "SourceGraphic"
                      , Svg.Attributes.type_ "matrix"
                      , Svg.Attributes.values (
                          "0.9 0   0   0   0.1 " ++
                          "0   0.9 0   0   0.1 " ++
                          "0   0   0.9 0   0.1 " ++
                          "0   0   0   1   0")
                      , Svg.Attributes.result "r1"
                      ]
                      []
                  , Svg.feBlend
                      [ Svg.Attributes.in_ "r0"
                      , Svg.Attributes.in2 "r1"
                      , Svg.Attributes.mode "multiply"
                      ]
                      []
                  ]
              ]
            ]
      )
      ++
      [ Svg.rect
          (
            [ Svg.Attributes.x "-0.5"
            , Svg.Attributes.y "-0.5"
            , Svg.Attributes.width "1"
            , Svg.Attributes.height "1"
            ]
            ++
            (
              case runner.pfpUrl of
                Nothing -> []
                Just _ -> [ Svg.Attributes.filter "url(#pfp)" ]
            )
          )
          []
      ]
    )

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
    , Svg.Attributes.height (String.fromFloat (canvasHeight - 0.5))
    ]
    []

--{-| Produce an `Svg` representation of the track. -}
trackView : Track -> Svg Msg
trackView track = Svg.g [] (List.map obstacleView track.obstacles)

--{-| Produce the title screen `Html`. -}
pausedView : Model -> Html Msg
pausedView model =
  Html.div
  [ Html.Attributes.style "display" "flex"
  , Html.Attributes.style "flex-direction" "column"
  , Html.Attributes.style "justify-content" "center"
  , Html.Attributes.style "align-items" "center"
  , Html.Attributes.style "width" "100%"
  , Html.Attributes.style "height" "100%"
  , Html.Attributes.style "color" model.theme.fg
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
    , Html.Attributes.style "height" "100%"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "flex-direction" "column"
    , Html.Attributes.style "justify-content" "center"
    , Html.Attributes.style "background-color" model.theme.fg
    , Html.Attributes.style "color" model.theme.bg
    , Html.Attributes.style "transition" "2s background-color, 4s color"
    , Html.Attributes.style "-webkit-user-select" "none"
    , Html.Attributes.style "-moz-user-select" "none"
    , Html.Attributes.style "-ms-user-select" "none"
    , Html.Attributes.style "-khtml-user-select" "none"
    ]
    [ Html.div
        [ Html.Attributes.style "font-size" "3vw"
        , Html.Attributes.style "margin" "1vw"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "space-between"
        ]
        [ Html.span []
            [ Html.text ("Score: " ++ (String.fromInt model.score)) ] 
        , Html.span []
            [ Html.text ("High score: " ++ (String.fromInt model.highScore)) ]
        ]
    , Html.div
        [ Html.Attributes.style "background-color" model.theme.bg
        , Html.Attributes.style "position" "relative"
        , Html.Attributes.style "transition" "4s background-color"
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
            , Svg.Attributes.fill model.theme.fg
            , Html.Attributes.style "transition" "2s fill"
            ]
            (if model.state == Playing then playingView model else [])
        , Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "right" "0"
            ]
            (if model.state == Paused then [pausedView model] else [])
        ]
        , Html.div
            [ Html.Attributes.style "font-size" "1.5vw"
            , Html.Attributes.style "text-align" "right"
            , Html.Attributes.style "margin-bottom" "6vw"
            , Html.Attributes.style "margin-right" "0.5vw"
            ]
            [ Html.text version ]
      ]

{-| The entry point of the application -}
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
