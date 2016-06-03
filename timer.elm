port module Timer exposing (main, setStorage, scrollBottom, scrambleReq, scrambleRes)
import Html exposing (Html, button, div, text, h1, ul, li, a, span, header, section, select, optgroup, option)
import Html.App as Html
import Html.Lazy as Html
import Html.Attributes exposing (class, attribute, href, title, value, selected)
import Time exposing (Time, second)
import Task
import Keyboard

import Scrambles
import History

import Util exposing ((<$>), pretty, toPrecision)

port setStorage : SerialModel -> Cmd msg
port scrollBottom : String -> Cmd msg


port scrambleReq : Scrambles.ScrType -> Cmd msg
port scrambleRes : (String -> msg) -> Sub msg

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
  , scrType : Scrambles.SerialModel
}

serialize : Model -> SerialModel
serialize m = SerialModel (History.serialize m.history) (Scrambles.serialize m.scramble)


type alias Model = {
  time : Time,
  startTime : Time,
  state : TimerState,
  totalInspection : Time,
  scramble : Scrambles.Model,
  history : History.Model
}


deserialize : SerialModel -> Model
deserialize st = Model 0 0 Stopped (15 * second) (Scrambles.deserialize st.scrType) (History.deserialize st.oldTimes)

empty : SerialModel
empty = SerialModel History.empty Scrambles.empty

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
withGetScramble = withCmd (\model -> scrambleReq model.scramble.scrtype )


-- UPDATE
type UpDown = Up | Down

type Msg
  = NoOp
  | Tick Time
  | StartTime Time
  | Space UpDown
  | HistoryMsg History.Msg
  | ScrambleMsg Scrambles.Msg

getCurTime : (Time -> Msg) -> Cmd Msg
getCurTime m = Task.perform m m Time.now

addOldTime : Model -> Model
addOldTime m =
    updateHistory m <| History.addTime {
      time = m.time,
      startTime = m.startTime,
      scramble = m.scramble
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
    ScrambleMsg msg ->
        let
          (newscrmbl, shouldupdate) = Scrambles.update msg model.scramble
        in
            if shouldupdate then
                withSetStorage <| withGetScramble <| upM {model | scramble = newscrmbl}
            else upM {model | scramble = newscrmbl}

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
    [ scrambleRes (ScrambleMsg << Scrambles.newscramble)
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

viewStats : History.Model -> Html Msg
viewStats history =
  let
    renderTimeSD : (Time, Float) -> String
    renderTimeSD (t, sd) = (pretty t) ++ " (σ=" ++ (toPrecision 2 sd) ++ ")"
    st = History.getStats history
    stat : String -> String -> Html Msg
    stat label content = div [ class "stat"] [
        span [class "stat_left"] [text label], span [] [text content]]
    statSec : String -> List (String, String) -> Html Msg 
    statSec head rows = div [class "stat_group"]
      ([ div [ class "stat_hdr" ] [text head] ] ++
        List.map (uncurry stat) rows)
    mapAvg : (Int, (Time, Float), (Time, Float)) -> Html Msg
    mapAvg (avgn, cur, best) =
      statSec ("avg" ++ toString avgn)
      [ ("current", renderTimeSD cur)
      , ("best", renderTimeSD best)]
  in
  section [ class ("stats") ]
  ([ header [] [text <| "stats (" ++ (toString st.totalFin) ++ "/" ++ (toString st.total) ++ ")"]
  , statSec "times"
    (List.filterMap identity
    [ ((,) "best" << pretty) <$> st.best
    , ((,) "worst" << pretty) <$> st.worst
    , ((,) "average" << renderTimeSD) <$> st.average
    , ((,) "mean" << pretty) <$> st.mean
    ])
  ] ++ (List.map mapAvg st.averages))

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
  [ Html.map ScrambleMsg <| Html.lazy Scrambles.view model.scramble
  , section [ class ("timerstate " ++ stateToClassName) ] [ h1 [] [ text timefmt ] ]
  , Html.map HistoryMsg  <| Html.lazy History.view model.history
  , Html.lazy viewStats model.history
  ]
