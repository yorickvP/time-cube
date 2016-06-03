port module History exposing (Model, SerialModel, serialize, deserialize, update, view, Msg, empty, addTime, getStats)

import Time exposing (Time, second)
import Html exposing (Html, text, ul, li, a, span, section)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, attribute, href, title)
import List
import String exposing (slice, padRight)
import Date
import Date.Format
import Scrambles
import Util exposing ((<$>))



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
  scrambleType : Scrambles.Model
}

type alias SerialOldTime = {
  time : Time,
  startTime : Time,
  comment : String,
  flag : Int,
  scrambleType : Scrambles.Model -- load from scrambles array?
}

type Flag = None | Penalty2 | DNF
type alias OldTimeID = Int
type alias Model = List OldTime
type alias SerialModel = List SerialOldTime

port alert : String -> Cmd msg


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
  | GetScramble OldTimeID


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

lookupTime : OldTimeID -> List OldTime -> Maybe OldTime
lookupTime id list = snd <$> List.head (List.indexedMap (,) list |> List.filter (fst >> ((==) id)))

  --  oldTime = History.OldTime m.time m.startTime "" History.None m.curScramble (Just m.scrambleType)
addTime : {time : Time, startTime: Time, scramble : Scrambles.Model} -> Model -> Model
addTime tm =
    let ot = OldTime tm.time tm.startTime "" None tm.scramble
    in (::) ot
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleFlag flag id ->
        (setFlag flag id model, Cmd.none)
    DeleteTime id ->
        (rmTime id model, Cmd.none)
    GetScramble id ->
        (model, alert <| Maybe.withDefault "" <| (Scrambles.toString << (.scrambleType)) <$> (lookupTime id model))


type alias Stats =
  { total : Int
  , totalFin : Int
  , best : Maybe Time
  , worst : Maybe Time
  , average : Maybe (Time, Float) -- SD
  , mean : Maybe Time
  , averages : List (Int, (Time, Float), (Time, Float))
  }

trimOutlying : List comparable -> List comparable
trimOutlying list =
    let
        length = List.length list
        sorted = List.sort list
        trim = ceiling (toFloat length / 20.0)
        trimmed = List.take (length - (2 * trim)) (List.drop trim sorted)
    in trimmed

average : List Time -> Time -- possible div/0
average times = List.sum times / (toFloat <| List.length times)

getAvgSD : List Time -> Maybe (Time, Float)
getAvgSD times =
    if List.length times < 3 then Nothing
    else let
        trimmed = trimOutlying times
        avg = average trimmed
        variance = average <| List.map (\x -> ((abs (x - avg)) / Time.second) ^ 2) trimmed
        sd = sqrt variance
    in Just (avg, sd)

mapMin : (a -> comparable) -> List a -> Maybe a
mapMin fun = List.head << List.sortBy fun

mapWithN : Int -> (List x -> y) -> List x -> List y
mapWithN n fun list =
  let
    nl = List.take n list
  in
    if (List.length nl) < n then []
    else (fun nl) :: (mapWithN n fun (List.drop 1 list))

getMaybes : List (Maybe a) -> List a
getMaybes = List.filterMap identity

-- impossible to do this generically for now
maybe_lift2 : (e -> f -> g) -> Maybe e -> Maybe f -> Maybe g
maybe_lift2 fun a b = a `Maybe.andThen` (\firsta -> b `Maybe.andThen` (Just << fun firsta))

getStats : Model -> Stats
getStats model = let
  notDNF = List.filter (.flag >> (/=) DNF) model
  times = List.map getTimeWithPenalty notDNF
  getAverage : Int -> Maybe (Int, (Time, Float))
  getAverage n = if List.length times < n then Nothing else
    Maybe.map ((,) n) (getAvgSD (List.take n times))
  getBestAvg : Int -> Maybe (Int, (Time, Float))
  getBestAvg n = if List.length times < n then Nothing else
    Maybe.map ((,) n) <|
    mapMin (fst) <| getMaybes <| mapWithN n getAvgSD times
  combineTuples (x, a) (y, b) = (x, a, b)
 in
  { total = List.length model
  , totalFin = List.length times
  , best = List.minimum <| times
  , worst = List.maximum <| times
  , mean =
        if List.isEmpty times
            then Nothing
            else Just <| average times
  , average = getAvgSD times
  , averages = getMaybes
    [ maybe_lift2 combineTuples (getAverage 5) (getBestAvg 5)
    , maybe_lift2 combineTuples (getAverage 12) (getBestAvg 12)
    , maybe_lift2 combineTuples (getAverage 50) (getBestAvg 50)
    , maybe_lift2 combineTuples (getAverage 100) (getBestAvg 100)
    ]
  }

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
    , a [class "getscramble", href "#", onClick (GetScramble i)] [text "scramble?"]
    , a [class "delete", href "#", onClick (DeleteTime i)] [text "x"]
    ]
  in
  section [ class ("oldtimes") ] [ ul [] (List.reverse <| List.indexedMap mapTime oldTimes) ]
