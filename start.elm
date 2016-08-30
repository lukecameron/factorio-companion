
import Html exposing (Html, Attribute, text, div, input, pre)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, int, string, dict, object5, object4, maybe, float, tuple2, (:=), at)
import Json.Decode as Json
import Result exposing (..)
import Basics


main =
  beginnerProgram { model = "", view = view, update = update }

-- PROCESS
decodeCount : Decoder Float
decodeCount =
  at ["count"] float

decodeIngredients : Decoder (List (String, Int))
decodeIngredients =
  Json.list (tuple2 (,) string int)

decodeRecipe : Decoder RecipeJson
decodeRecipe =
  object5 RecipeJson
    ("name" := string)
    ("result" := string)
    ("ingredients" := decodeIngredients)
    ("type" := string)
    (maybe ("energy_required" := float))

decodeAllRecipes : Decoder (Dict String (Maybe RecipeJson))
decodeAllRecipes = dict (maybe decodeRecipe)

type alias RecipeJson = {
  name : String,
  result : String,
  ingredients : List (String, Int),
  recipeType : String,
  energy_required : Maybe Float
}

parseAndTransform text = case Json.decodeString decodeAllRecipes text of
  Ok ob -> Basics.toString ob
  Err message -> message

-- UPDATE
type Msg = JSONChanged String

update (JSONChanged content) oldContent = content

-- VIEW
view content =
  div []
    [ input [ placeholder "JSON in", onInput JSONChanged, myStyle ] []
    , pre [ myStyle ] [ text (parseAndTransform content) ]
    ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
--    , ("text-align", "center")
    ]
