port module Scrambles exposing (ScrType, scrambles, lookup, empty, deserialize)

type alias ScrType =
  { name : String
  , val : String
  , len : Int }

type alias ScrGroup = {
    name : String,
    content : List (String, List ScrType)
}

maybeFilter : (x -> Bool) -> Maybe x -> Maybe x
maybeFilter fn x = if Maybe.withDefault False (Maybe.map fn x) then x else Nothing

lookup : String -> Maybe (String, ScrType)
lookup str =
  let
    mapInList : (a -> Maybe b) -> List a -> Maybe b
    mapInList cnd = Maybe.oneOf << List.map cnd
    findInList cnd = mapInList (maybeFilter cnd << Just)
    mapList : List ScrType -> Maybe ScrType
    mapList = findInList (.val >> (==) str)
    lookupInGroup : ScrGroup -> Maybe (String, ScrType)
    lookupInGroup grp = mapInList (\(s, v) -> Maybe.map ((,) s) (mapList v)) grp.content
  in mapInList lookupInGroup scrambles

empty : String
empty = "333"

default : ScrType
default = ScrType "random state" "333" 0

deserialize : String -> ScrType
deserialize str = Maybe.withDefault default (Maybe.map snd <| lookup str)

scrambles : List ScrGroup
scrambles =
  [ ScrGroup "WCA PUZZLES" [
      ("2x2x2", [
        ScrType "random state" "222so" 11,
        ScrType "optimal random state" "222o" 0,
        ScrType "3-gen" "2223" 25,
        ScrType "6-gen" "2226" 25]),
      ("3x3x3", [
        default, -- ScrType "random state" "333" 0,
        ScrType "random state + orientation" "333ori" 0,
        ScrType "old style" "333o" 25]),
      ("4x4x4", [
        ScrType "SiGN" "444" 40,
        ScrType "WCA" "444wca" 40,
        ScrType "YJ (place fixed center on Urf)" "444yj" 40]),
      ("5x5x5", [
        ScrType "SiGN" "555" 60,
        ScrType "WCA" "555wca" 60]),
      ("6x6x6", [
        ScrType "SiGN" "666si" 80,
        ScrType "prefix" "666p" 80,
        ScrType "suffix" "666s" 80]),
      ("7x7x7", [
        ScrType "SiGN" "777si" 100,
        ScrType "prefix" "777p" 100,
        ScrType "suffix" "777s" 100]),
      ("Clock", [
        ScrType "Jaap order" "clk" 0,
        ScrType "concise" "clkc" 0,
        ScrType "efficient pin order" "clke" 0,
        ScrType "WCA" "clkwca" 0]),
      ("Megaminx", [
        ScrType "Pochmann" "mgmp" 70,
        ScrType "old style" "mgmo" 70]),
      ("Pyraminx", [
        ScrType "random state" "pyrso" 11,
        ScrType "optimal random state" "pyro" 0,
        ScrType "random moves" "pyrm" 25]),
      ("Skewb", [
        ScrType "random state" "skbso" 11,
        ScrType "optimal random state" "skbo" 0,
        ScrType "U L R B" "skb" 25]),
      ("Square-1", [
        ScrType "random state" "sqrs" 0,
        ScrType "face turn metric" "sq1h" 40,
        ScrType "twist metric" "sq1t" 20])
    ],
    ScrGroup "OTHER PUZZLES" [
      ("8 puzzle", [
        ScrType "random state (fast)" "8puzso" 0,
        ScrType "optimal random state" "8puzo" 0]),
      ("15 puzzle", [
        ScrType "random state (fast)" "15puzso" 0,
        ScrType "random state (efficient)" "15puzsoe" 0,
        ScrType "random moves" "15p" 80]),
      ("24 puzzle", [
        ScrType "random state (fast)" "24puzso" 0]),
      ("1x3x3 (Floppy Cube)", [
        ScrType "U D L R" "flp" 25]),
      ("2x3x3 (Domino)", [
        ScrType " " "223" 25]),
      ("3x3x4", [ScrType " " "334" 40]),
      ("3x3x5", [ScrType "shapeshifting" "335" 25]),
      ("3x3x6", [ScrType " " "336" 40]),
      ("3x3x7", [ScrType "shapeshifting" "337" 40]),
      ("4x4x6", [ScrType " " "446" 40]),
      ("8x8x8", [ScrType "SiGN" "888" 120]),
      ("9x9x9", [ScrType "SiGN" "999" 120]),
      ("10x10x10", [ScrType "SiGN" "101010" 120]),
      ("11x11x11", [ScrType "SiGN" "111111" 120]),
      ("Cmetrick", [ScrType " " "cm3" 25]),
      ("Cmetrick Mini", [ScrType " " "cm2" 25]),
      ("Domino (2x3x3)", [ScrType " " "223" 25]),
      ("Floppy Cube (1x3x3)", [ScrType "U D L R" "flp" 25]),
      ("FTO (Face-Turning Octahedron)", [ScrType " " "fto" 25]),
      ("Gigaminx", [ScrType "Pochmann" "giga" 300]),
      ("Helicopter Cube", [ScrType " " "heli" 40]),
      ("Pyraminx Crystal", [
        ScrType "Pochmann" "prcp" 70,
        ScrType "old style" "prco" 70]),
      ("Siamese Cube (1x1x3 block)", [ScrType " " "sia113" 25]),
      ("Siamese Cube (1x2x3 block)", [ScrType " " "sia123" 25]),
      ("Siamese Cube (2x2x2 block)", [ScrType " " "sia222" 25]),
      ("Square-2", [ScrType " " "sq2" 20]),
      ("Super Floppy Cube", [ScrType " " "sfl" 25]),
      ("Super Square-1", [ScrType "twist metric" "ssq1t" 20]),
      ("UFO", [ScrType "Jaap style" "ufo" 25])
    ],
    ScrGroup "SPECIALTY SCRAMBLES" [
      ("1x1x1", [
        ScrType "x y z" "111" 25]),
      ("3x3x3 subsets", [
        ScrType "2-generator <R,U>" "2gen" 25,
        ScrType "2-generator <L,U>" "2genl" 25,
        ScrType "Roux-generator <M,U>" "roux" 25,
        ScrType "3-generator <F,R,U>" "3gen_F" 25,
        ScrType "3-generator <R,U,L>" "3gen_L" 25,
        ScrType "3-generator <R,r,U>" "RrU" 25,
        ScrType "half turns only" "half" 25,
        ScrType "edges only" "edges" 0,
        ScrType "corners only" "corners" 0,
        ScrType "last layer" "ll" 0,
        ScrType "CMLL+LSE" "cmll" 0,
        ScrType "last slot + last layer" "lsll2" 0,
        ScrType "ZBLL" "zbll" 0,
        ScrType "2GLL" "2gll" 0,
        ScrType "PLL" "pll" 0,
        ScrType "ZZ last slot + last layer" "zzls" 0,
        ScrType "last slot + last layer (old)" "lsll" 15]),
      ("Bandaged Cube (Bicube)", [
        ScrType "" "bic" 30]),
      ("Bandaged Square-1 </,(1,0)>", [
        ScrType "twist metric" "bsq" 25]),
      ("Bigcube subsets", [
        ScrType "<R,r,U,u>" "RrUu" 40,
        ScrType "4x4x4 edges" "4edge" 8,
        ScrType "5x5x5 edges" "5edge" 8,
        ScrType "6x6x6 edges" "6edge" 8,
        ScrType "7x7x7 edges" "7edge" 8]),
      ("Megaminx subsets", [
        ScrType "2-generator <R,U>" "minx2g" 30,
        ScrType "last slot + last layer" "mlsll" 20]),
      ("Relays", [
        ScrType "lots of 3x3x3s" "r3" 5,
        ScrType "234 relay" "r234" 0,
        ScrType "2345 relay" "r2345" 0,
        ScrType "23456 relay" "r23456" 0,
        ScrType "234567 relay" "r234567" 0,
        ScrType "Guildford Challenge" "guildford" 0,
        ScrType "Mini Guildford Challenge" "miniguild" 0])
    ],
    ScrGroup "JOKE SCRAMBLES" [
      ("-1x-1x-1 (micro style)", [ScrType " " "-1" 25]),
      ("1x1x2", [ScrType " " "112" 25]),
      ("3x3x3 for noobs", [ScrType " " "333noob" 25]),
      ("LOL", [ScrType " " "lol" 25]),
      ("Derrick Eide", [ScrType " " "eide" 25])
    ]
  ]
