port module History exposing (Model, SerialModel, serialize, deserialize, update, view, Msg, empty, addTime)

import Time exposing (Time, second)
import Html exposing (Html, text, ul, li, a, span, section)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, attribute, href, title)
import List
import String exposing (slice, padRight)
import Date
import Date.Format


-- MODEL
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

type alias OldTime = {
  time : Time,
  startTime : Time,
  comment : String,
  flag : Flag,
  scramble : Maybe String,
  scrambleType : Maybe ScrambleType
}

type alias SerialOldTime = {
  time : Time,
  startTime : Time,
  comment : String,
  flag : Int,
  scramble : Maybe String,
  scrambleType : Maybe ScrambleType
}


type alias ScrambleType =
  { len : Int
  , scrType : String 
  }
type Flag = None | Penalty2 | DNF
type alias OldTimeID = Int
type alias Model = List OldTime
type alias SerialModel = List SerialOldTime

serializeTime : OldTime -> SerialOldTime
serializeTime t = {t | flag = mapFlagtoint t.flag}
deserializeTime : SerialOldTime -> OldTime
deserializeTime t = {t | flag = mapintToFlag t.flag}


serialize : Model -> SerialModel
serialize = List.map serializeTime


deserialize : SerialModel -> Model
deserialize = List.map deserializeTime

empty : SerialModel
empty = []

-- UPDATE
type Msg
  = ToggleFlag Flag OldTimeID
  | DeleteTime OldTimeID


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

  --  oldTime = History.OldTime m.time m.startTime "" History.None m.curScramble (Just m.scrambleType)
addTime : {time : Time, startTime: Time, scramble : Maybe String, scrambleType : ScrambleType} -> Model -> Model
addTime tm =
    let ot = OldTime tm.time tm.startTime "" None tm.scramble (Just tm.scrambleType)
    in (::) ot
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleFlag flag id ->
        (setFlag flag id model, Cmd.none)
    DeleteTime id ->
        (rmTime id model, Cmd.none)

-- view


floatPart : Float -> Float
floatPart i = i - (toFloat <| truncate i)


pretty : Time -> String
pretty time =
  let x = (toFloat <| floor (time / 10)) / 100
  in (toString <| truncate x) ++ "." ++ (padRight 2 '0' <| slice 2 4 <| toString <| floatPart x)


getTimeWithPenalty : OldTime -> Time
getTimeWithPenalty time = time.time + if time.flag == Penalty2 then second * 2 else 0

view : Model -> Html Msg
view oldTimes = let
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
  mapTime i t = li
    [ attribute "data-id" (toString i)
    , class (flagsToClassName t)
    , title <| Date.Format.format "%d-%m-%Y %H:%M:%S" (Date.fromTime (t.startTime + t.time))
    ]
    [ span [class "textlabel"] [text <| pretty <| getTimeWithPenalty t]
    , flagElm Penalty2 i t "+2"
    , flagElm DNF i t "DNF"
    , a [class "delete", href "#", onClick (DeleteTime i)] [text "x"]
    ]
  in
  section [ class ("oldtimes") ] [ ul [] (List.reverse <| List.indexedMap mapTime oldTimes) ]
