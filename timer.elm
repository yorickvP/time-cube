port module Timer exposing (main, setStorage, scrollBottom, scrambleReq, scrambles)
import Html exposing (Html, button, div, text, h1, ul, li, a, span, header, section)
import Html.App as Html
import Html.Attributes exposing (class, attribute, href, title)
import Time exposing (Time, second)
import String exposing (slice, padRight)
import Task
import Keyboard
import History


port setStorage : SerialModel -> Cmd msg
port scrollBottom : String -> Cmd msg

port scrambleReq : ScrambleType -> Cmd msg
port scrambles : (String -> msg) -> Sub msg


main : Program (Maybe SerialModel)
main =
  Html.programWithFlags
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions }


-- MODEL

-- waiting is a hack for the time between keyup and keydown, really
type TimerState = Stopped | Running | Inspecting | Waiting
type alias SerialModel = {
  oldTimes : History.SerialModel
}

serialize : Model -> SerialModel
serialize m = SerialModel <| History.serialize m.history


type alias ScrambleType =
  { len : Int
  , scrType : String 
  }

type alias Model = {
  time : Time,
  startTime : Time,
  state : TimerState,
  totalInspection : Time,
  curScramble : Maybe String,
  scrambleType : ScrambleType,
  history : History.Model
}

deserialize : SerialModel -> Model
deserialize st = Model 0 0 Stopped (15 * second) Nothing (ScrambleType 0 "333") (History.deserialize st.oldTimes)

empty : SerialModel
empty = SerialModel History.empty

init : Maybe SerialModel -> (Model, Cmd Msg)
init local_storage =
  let
    stored : SerialModel
    stored = Maybe.withDefault empty local_storage
  in
    withGetScramble (deserialize stored, Cmd.none)

withCmd : (Model -> Cmd Msg) -> (( Model, Cmd Msg ) -> ( Model, Cmd Msg ))
withCmd makeCmd (model, cmds) = (model, Cmd.batch [makeCmd model, cmds])

withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage = withCmd (setStorage << serialize)

withGetScramble : (Model, Cmd Msg) -> (Model, Cmd Msg)
withGetScramble = withCmd (scrambleReq << (.scrambleType))


-- UPDATE
type UpDown = Up | Down

type Msg
  = NoOp
  | Tick Time
  | StartTime Time
  | Space UpDown
  | HistoryMsg History.Msg
  | Scramble String

getCurTime : (Time -> Msg) -> Cmd Msg
getCurTime m = Task.perform m m Time.now

addOldTime : Model -> Model
addOldTime m =
    updateHistory m <| History.addTime {
      time = m.time,
      startTime = m.startTime,
      scramble = m.curScramble,
      scrambleType = m.scrambleType
    } m.history

scrollOldTimes : Cmd Msg
scrollOldTimes = scrollBottom ".oldtimes"

const : a -> b -> a
const a b = a

updateHistory : Model -> History.Model -> Model
updateHistory m newhist = {m | history = newhist}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    upM m = (m, Cmd.none)
    donothing = upM model
  in
  case msg of
    NoOp -> donothing
    Tick t ->
      upM { model | time = t - model.startTime }
    StartTime t ->
      upM { model | time = 0, startTime = t, state = Inspecting}
    Space Down ->
      case model.state of
        Running ->
          withGetScramble <| withSetStorage <| (addOldTime { model | state = Waiting }, scrollOldTimes)
        Inspecting ->
          upM { model | state = Running, startTime = model.time + model.startTime, time = 0 }
        _ -> donothing
    Space Up ->
      case model.state of
        Stopped -> (model, getCurTime StartTime)
        Waiting -> upM {model | state = Stopped }
        _ -> donothing
    HistoryMsg msg ->
      let
        (newhist, histmsg) = History.update msg model.history 
      in
        withSetStorage <| ({model | history = newhist}, Cmd.map HistoryMsg histmsg)
    Scramble scr ->
      upM {model | curScramble = Just scr}

keyupdown : UpDown -> Int -> Msg
keyupdown updown keycode =
  if keycode == 32
    then Space updown
    else NoOp

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions m =
 let
  shouldTick = ((m.state == Running) || (m.state == Inspecting))
 in
  Sub.batch
    [ scrambles Scramble
    , Keyboard.downs (keyupdown Down)
    , Keyboard.ups (keyupdown Up)
    , (if shouldTick then
        Time.every (second / 100) Tick
      else Sub.none)
    ]
-- VIEW

getRunTime : Model -> Float
getRunTime m = (toFloat <| round (m.time / 10)) / 100

getInspectTime : Model -> Int
getInspectTime m = ceiling ((m.totalInspection - m.time) / 1000)

floatPart : Float -> Float
floatPart i = i - (toFloat <| truncate i)

pretty : Time -> String
pretty time =
  let x = (toFloat <| floor (time / 10)) / 100
  in (toString <| truncate x) ++ "." ++ (padRight 2 '0' <| slice 2 4 <| toString <| floatPart x)

view : Model -> Html Msg
view model = let
  timefmt = case model.state of
    Stopped -> if model.startTime == 0 then "Ready" else pretty model.time
    Inspecting -> toString <| getInspectTime model
    _ -> pretty model.time
  stateToClassName = case model.state of
    Stopped -> "stopped"
    Inspecting -> "inspecting"
    Running -> "running"
    Waiting -> "stopped"
  in
  div [class "container"]
  [ header []
    [ h1 [] [ text "yyTimer" ]
    , div [class "scramble"] [ text <| Maybe.withDefault "loading" model.curScramble ] ]
  , section [ class ("timerstate " ++ stateToClassName) ] [ h1 [] [ text timefmt ] ]
  , Html.map HistoryMsg (History.view model.history)
  , section [ class ("stats") ] [ text "stats here" ]
  ]
