port module Util exposing ((<$>), (<**>), label, onChange, pretty, toPrecision)

import Html
import Html.Attributes exposing (attribute, value, selected)
import Html.Events as Html
import Json.Decode as Json
import Time exposing (Time, second)
import String exposing (slice, padRight)


(<$>) : (a -> b) -> Maybe a -> Maybe b
(<$>) = Maybe.map

(<**>) : Maybe a -> (a -> b) -> Maybe b
(<**>) = flip (<$>)


label : String -> Html.Attribute a
label = attribute "label"

onChange : (String -> a) -> Html.Attribute a
onChange tg = Html.on "change" (Json.map tg Html.targetValue)


pretty : Time -> String
pretty time =
  let x = (toFloat <| floor (time / 10)) / 100
  in toPrecision 2 x


floatPart : Float -> Float
floatPart i = i - (toFloat <| truncate i)

toPrecision : Int -> Float -> String
toPrecision n x =
  (toString <| truncate x) ++ "." ++ (padRight n '0' <| slice 2 (2+n) <| toString <| floatPart x)
