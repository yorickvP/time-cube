port module Timer exposing (..)
import Html exposing (Html, button, div, text, h1, ul, li, a, span, header, section)
import Html.App as Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, attribute, href)
import Time exposing (Time, second)
import List
import String exposing (slice, padRight)
import Json.Decode as JSOND
import Json.Encode as JSONE
import Json.Decode exposing ((:=))
-- import Json.Decode as Json
import Task
import Result

port keyupdown : ((Int, Int) -> msg) -> Sub msg

port setStorage : JSONE.Value -> Cmd msg

main : Program (JSOND.Value)
main =
  Html.programWithFlags
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions }


-- MODEL

-- waiting is a hack for the time between keyup and keydown, really
type TimerState = Stopped | Running | Inspecting | Waiting
type Flag = None | Penalty2 | DNF
type alias OldTimeID = Int

type alias StoredState = {
  oldTimes : List OldTime
}

mapintToFlag : Int -> Flag
mapintToFlag i = case i of
  1 -> Penalty2
  2 -> DNF
  _ -> None
mapFlagtoint : Flag -> Int
mapFlagtoint f = case f of
  None -> 0
  Penalty2 -> 1
  DNF -> 2
deSerialize : JSOND.Decoder StoredState
deSerialize = JSOND.object1 StoredState ("oldTimes" := JSOND.list deSerializeTime)
deSerializeTime : JSOND.Decoder OldTime
deSerializeTime = JSOND.object4 OldTime
  ("time" := JSOND.float)
  ("startTime" := JSOND.float)
  ("comment" := JSOND.string)
  ("flag" := JSOND.map mapintToFlag JSOND.int)

serialize : StoredState -> JSONE.Value
serialize state =
  let
    serializeTime : OldTime -> JSONE.Value
    serializeTime t = JSONE.object
      [ ("time", JSONE.float t.time)
      , ("startTime", JSONE.float t.startTime)
      , ("comment", JSONE.string t.comment)
      , ("flag", JSONE.int <| mapFlagtoint t.flag)
      ]
  in
    JSONE.object [("oldTimes", JSONE.list <| List.map serializeTime state.oldTimes)]

serializeModel : Model -> JSONE.Value
serializeModel m = serialize <| StoredState m.oldTimes

type alias OldTime = {
  time : Time,
  startTime : Time,
  comment : String,
  flag : Flag
}

type alias Model = {
  time : Time,
  startTime : Time,
  state : TimerState,
  totalInspection : Time,
  oldTimes : List OldTime
}

emptyModel : List OldTime -> Model
emptyModel oldtimes = Model 0 0 Stopped (15 * second) oldtimes

init : JSOND.Value -> (Model, Cmd Msg)
init local_storage =
  let
    stored : StoredState
    stored = Result.withDefault (StoredState []) (JSOND.decodeValue deSerialize local_storage)
  in
    (emptyModel stored.oldTimes, Cmd.none)

withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage (model, cmds) =
  ( model, Cmd.batch [ setStorage <| serializeModel model, cmds ] )

-- UPDATE

type Msg
  = Tick Time
  | StartTime Time
  | Toggle (Int, Int)
  | ToggleFlag Flag OldTimeID
  | DeleteTime OldTimeID

getCurTime : (Time -> Msg) -> Cmd Msg
getCurTime m = Task.perform m m Time.now

spacekey : Int
spacekey = 13

addOldTime : Model -> Model
addOldTime m =
  let
    oldTime : OldTime
    oldTime = OldTime m.time m.startTime "" None
  in
    { m | oldTimes = oldTime :: m.oldTimes }

setFlag : Flag -> OldTimeID -> List OldTime -> List OldTime
setFlag fl id otm =
  let
    m : OldTimeID -> OldTime -> OldTime
    m i oldtime =
      if i == id then
        {oldtime | flag = (if oldtime.flag == fl then None else fl)}
      else oldtime
  in
    List.indexedMap m otm

rmTime : OldTimeID -> List OldTime -> List OldTime
rmTime id times = (List.take id times) ++ (List.drop (id + 1) times)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    donothing = (model, Cmd.none)
  in
  case msg of
    Tick t -> ({ model | time = t - model.startTime }, Cmd.none)
    StartTime t -> ({ model | time = 0, startTime = t, state = Inspecting}, Cmd.none)
    Toggle (32, 0) -> -- key down
      case model.state of
        Running -> withSetStorage (addOldTime { model | state = Waiting}, Cmd.none) -- TODO: scroll stats to bottom
        Inspecting -> ({ model | state = Running, startTime = model.time + model.startTime, time = 0 }, Cmd.none)
        _ -> donothing
    Toggle (0, 32) -> -- key up
      case model.state of
        Stopped -> (model, getCurTime StartTime)
        Waiting -> ({model | state = Stopped }, Cmd.none)
        _ -> donothing
    Toggle (_, _) -> donothing
    ToggleFlag flag id -> withSetStorage ({model | oldTimes = setFlag flag id model.oldTimes}, Cmd.none)
    DeleteTime id -> withSetStorage ({model | oldTimes = rmTime id model.oldTimes}, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions m = let
  shouldTick = ((m.state == Running) || (m.state == Inspecting))
 in
  Sub.batch [ keyupdown Toggle, 
 (if shouldTick then Time.every (second / 100) Tick
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

getTimeWithPenalty : OldTime -> Time
getTimeWithPenalty time = time.time + if time.flag == Penalty2 then second * 2 else 0

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
  flagsToClassName : OldTime -> String
  flagsToClassName t = case t.flag of
    None -> ""
    Penalty2 -> "penalty2"
    DNF -> "dnf"
  flagElm : Flag -> OldTimeID -> OldTime -> String -> Html Msg
  flagElm fl i tm txt =
    a [ class <| "flag" ++ if tm.flag == fl then " active" else ""
      , href "#", onClick (ToggleFlag fl i)] [text txt]
  mapTime : OldTimeID -> OldTime -> Html Msg
  mapTime i t = li [attribute "data-id" (toString i), class (flagsToClassName t)]
    [ span [class "textlabel"] [text <| pretty <| getTimeWithPenalty t]
    , flagElm Penalty2 i t "+2"
    , flagElm DNF i t "DNF"
    , a [class "delete", href "#", onClick (DeleteTime i)] [text "x"]
    ]
  in
  div [class "container"]
  [ header [] [h1 [] [ text "yyTimer" ] ]
  , section [ class ("timerstate " ++ stateToClassName) ] [ h1 [] [ text timefmt ] ]
  , section [ class ("oldtimes") ] [ ul [] (List.reverse <| List.indexedMap mapTime model.oldTimes) ]
  , section [ class ("stats") ] [ text "stats here" ]
  ]
